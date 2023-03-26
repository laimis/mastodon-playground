namespace MastodonPlayground

module MastodonClientAdapter =

    let private dayCutoff = 60
    let private instanceUrl = "https://mstdn.social"

    let getPosts accessToken username =
        
        let client = new Mastonet.MastodonClient(instanceUrl, accessToken)

        let results =
            client.SearchAccounts(username)
            |> Async.AwaitTask
            |> Async.RunSynchronously

        match results.Count with
        | 0 -> 
            printfn "No user with username %s found" username
            None
        | _ -> // good to go

        let account =
            results
            |> Seq.take 1 |> Seq.last

        let callAndReturn maxId =

            let opt = Mastonet.ArrayOptions()
            match maxId with
            | Some id -> opt.MaxId <- id
            | None -> ()

            let onlyMedia = false
            let excludeReplies = true
            let pinned = false
            let excludeReblogs = false

            client.GetAccountStatuses(
                (account.Id),opt,onlyMedia,excludeReplies,pinned,excludeReblogs
            )

        let rec getPosts verbose gatheredPosts nextPostId=
                
                if verbose then
                    printfn
                        "Getting posts with %s, count %i"
                        (nextPostId |> Option.map string |> Option.defaultValue "None")
                        (gatheredPosts |> List.length)

                let posts = callAndReturn nextPostId |> Async.AwaitTask |> Async.RunSynchronously
                
                let allPosts = List.append gatheredPosts (posts |> List.ofSeq)

                let noMorePosts = posts.Count = 0
                let enoughOfPosts = allPosts.Length >= 1000
            
                match (noMorePosts, enoughOfPosts) with
                | (true, _) -> 
                    printfn "No more posts"
                    allPosts
                | (_, true) ->
                    printfn "Enough of posts"
                    allPosts
                | _ ->
                    let lastPost = posts |> Seq.last
                    let olderThan30Days = lastPost.CreatedAt < System.DateTime.Now.AddDays(-dayCutoff)
                    match olderThan30Days with
                    | true ->
                        printfn $"Reached older than {dayCutoff} days"
                        allPosts
                    | false ->
                        getPosts verbose allPosts (Some lastPost.Id)

        printfn "Getting posts"

        let posts = getPosts true [] None

        Some posts