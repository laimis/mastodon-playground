namespace MastodonPlayground

module TimelineAnalysis =

    let run accessToken username =
        
        let postsOption = MastodonClientAdapter.getPosts accessToken username MastodonClientAdapter.ExcludeReplies

        let posts =
            match postsOption with
            | None -> 
                printfn "No posts found"
                []
            | Some posts -> posts
        
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

        System.Console.WriteLine($"Average: {avg}, Total: {posts |> Seq.length}")