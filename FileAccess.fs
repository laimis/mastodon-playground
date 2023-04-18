namespace MastodonPlayground

module FileAccess =

    let private toFilenameWithPostfix (postfix:string) (username:string) =
        System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"posts-{username}-{postfix}.json"
        )

    let private toFilenameMinuteGranularity = 
        // filename should include the timestamp so that we can refresh the data
        let postfix = System.DateTime.Now.ToString("yyyyMMddHHmm")
        toFilenameWithPostfix postfix
    
    let private toFilenameLatest = toFilenameWithPostfix "latest"

    let private getUserPostsUsingFilename filenameFunc username =
        let postsFile = filenameFunc username
        match System.IO.File.Exists(postsFile) with
        | true ->
            printfn "Loading posts from file %s" postsFile
            Some (System.IO.File.ReadAllText(postsFile))
        | false ->
            None

    let internal getUserPosts =
        getUserPostsUsingFilename toFilenameMinuteGranularity

    let internal getUserPostsLastFetchTime =
        getUserPostsUsingFilename toFilenameLatest

    let internal saveUserPosts username content =
        [
            toFilenameMinuteGranularity // save it with minute frequency
            toFilenameLatest            // save it with latest
        ]
        |> List.map (fun filenameFunc -> filenameFunc username)
        |> List.iter (fun postsFile ->
            printfn "Saving posts to file %s" postsFile
            System.IO.File.WriteAllText(postsFile, content)
        )