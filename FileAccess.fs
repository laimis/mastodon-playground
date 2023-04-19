namespace MastodonPlayground

module internal FileAccess =

    let getTempDirectoryPath (subdirectory:string) =
        let tempPath = System.IO.Path.GetTempPath()
        let path = System.IO.Path.Combine(tempPath, subdirectory)
        // System.IO.Directory.CreateDirectory(path)
        path
        
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

    let getUserPosts =
        getUserPostsUsingFilename toFilenameMinuteGranularity

    let getUserPostsLastFetchTime =
        getUserPostsUsingFilename toFilenameLatest

    let saveUserPosts username content =
        [
            toFilenameMinuteGranularity // save it with minute frequency
            toFilenameLatest            // save it with latest
        ]
        |> List.map (fun filenameFunc -> filenameFunc username)
        |> List.iter (fun postsFile ->
            printfn "Saving posts to file %s" postsFile
            System.IO.File.WriteAllText(postsFile, content)
        )