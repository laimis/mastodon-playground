namespace MastodonPlayground

module TimelineAnalysis =

    let run accessToken username =
        
        let localCache =
            match Configuration.doUseCache with
            | true ->
                printfn "Getting posts from cache"
                FileAccess.getUserPostsLastFetchTime username
            | false ->
                printfn "Getting posts from Mastodon"
                None

        let postsOption =
            match localCache with
            | Some content ->
                Some (content |> Serialization.deserialize<List<Mastonet.Entities.Status>>)
            | None ->
                MastodonClientAdapter.getPosts accessToken username

        let posts =
            match postsOption with
            | None -> 
                printfn "No posts found"
                []
            | Some unwrapped -> unwrapped
        
        // group by date

        let postsGroupedByDate =
            posts
            |> Seq.groupBy (fun x -> x.CreatedAt.Date)


        // we want to print out the number of posts per day, represented with * characters
        // to make it look nice, we will determine how many posts is represented by 1 * character
        // we will use the average number of posts per day as the number of posts represented by 1 * character
        // this way, we can print out the number of posts per day in a nice way

        // get the average number of posts per day
        let avg =
            postsGroupedByDate
            |> Seq.map (fun (date, posts) -> posts |> Seq.length |> float)
            |> Seq.average

        let maxStars = 20.0

        let postsPerStar =
            postsGroupedByDate
            |> Seq.map (fun (_, posts) -> posts |> Seq.length |> float |> (/) avg |> int)
            |> Seq.max
            |> float |> (/) maxStars

        System.Console.WriteLine("Posts per star: " + postsPerStar.ToString("0.00"))

        postsGroupedByDate
        |> Seq.rev
        |> Seq.iter (fun (date, posts) ->
            let dateStr = date.ToString("yyyy-MM-dd")
            let numberOfPosts = posts |> Seq.length |> float
            let numStars = numberOfPosts / postsPerStar
            let stars = Array.init (System.Math.Ceiling(numStars) |> int) (fun _ -> "*") |> String.concat ""
            System.Console.WriteLine($"{dateStr}: {stars}")
        )

        System.Console.WriteLine($"Average: {avg}, Total: {posts |> Seq.length}")