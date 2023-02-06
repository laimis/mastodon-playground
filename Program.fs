open Microsoft.Extensions.Configuration

type Something = { Name: string; Value: int }

let builder = new ConfigurationBuilder()
builder.AddUserSecrets<Something>() |> ignore // had to create this type to get it to work
let config = builder.Build()
let accessToken = config["accesstoken"]

match accessToken with
| null -> 
    printfn "No access token, please add it to user secrets using 'dotnet user-secrets set \"accesstoken\" \"<value>\"'"
    System.Environment.Exit(1)
| _ -> // good to go


System.Console.WriteLine("Enter username:")
let username = System.Console.ReadLine()

let client = new Mastonet.MastodonClient("https://mstdn.social", accessToken)

let results =
    client.SearchAccounts(username)
    |> Async.AwaitTask
    |> Async.RunSynchronously

match results.Count with
| 0 -> 
    printfn "No user with username %s found" username
    System.Environment.Exit(1)
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

let rec getPosts gatheredPosts nextPostId =
        
        printfn
            "Getting posts with %s, count %i"
            (nextPostId |> Option.map string |> Option.defaultValue "None")
            (gatheredPosts |> List.length)

        let posts = callAndReturn nextPostId |> Async.AwaitTask |> Async.RunSynchronously
        let lastPost = posts |> Seq.last

        let allPosts = List.append gatheredPosts (posts |> List.ofSeq)

        let olderThan30Days = lastPost.CreatedAt < System.DateTime.Now.AddDays(-30.0)
        let noMorePosts = posts.Count = 0
        let enoughOfPosts = allPosts.Length >= 1000
    
        match (noMorePosts, olderThan30Days, enoughOfPosts) with
        | (true, _, _) -> 
            printfn "No more posts"
            allPosts
        | (_, true, _) ->
            printfn "Older than 30 days"
            allPosts
        | (_, _, true) ->
            printfn "Enough of posts"
            allPosts
        | _ ->
            getPosts allPosts (Some lastPost.Id)

let posts = getPosts [] None
 
// group by date

let postsGroupedByDate =
    posts
    |> Seq.groupBy (fun x -> x.CreatedAt.Date)


postsGroupedByDate
|> Seq.iter (fun (date, posts) ->
    let dateStr = date.ToString("yyyy-MM-dd")
    System.Console.WriteLine($"{dateStr}: {posts |> Seq.length}")
)

// get the average number of posts per day
let avg =
    postsGroupedByDate
    |> Seq.map (fun (date, posts) -> posts |> Seq.length |> float)
    |> Seq.average

System.Console.WriteLine($"Average: {avg}")