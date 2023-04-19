open Microsoft.Extensions.Configuration

type Something = { Name: string; Value: int }

type AccessToken = string
type Username = string
type SearchTerm = string

type Command =
    | PrintHelp
    | PrintHelpWithError of string
    | TimelineAnalysis of AccessToken * Username
    | PostSearch of AccessToken * Username * SearchTerm
    | PostLuceneIndex of AccessToken * Username
    | PostLuceneSearch of SearchTerm

let retrieveAccessToken() =
    let builder = new ConfigurationBuilder()
    builder.AddUserSecrets<Something>() |> ignore // had to create this type to get it to work
    let config = builder.Build()
    config["accesstoken"]

let printUsage() =
    printfn "Usage: dotnet run <command> <search term>"
    printfn "Commands:"
    printfn "  timeline - Analyze the timeline of a user"
    printfn "  search - Search for a term in a user's posts"

let printError (error:string) =
    let originalColor = System.Console.ForegroundColor;
    System.Console.ForegroundColor <- System.ConsoleColor.Red;
    System.Console.Error.WriteLine(error);
    System.Console.ForegroundColor <- originalColor;

let accessToken = retrieveAccessToken()
match accessToken with
| null -> 
    printfn "No access token, please add it to user secrets using 'dotnet user-secrets set \"accesstoken\" \"<value>\"'"
    System.Environment.Exit(1)
| _ -> // good to go

let collectUsername() =
    System.Console.WriteLine("Enter username:")
    let username = System.Console.ReadLine().Trim()

    match username with
    | "" -> 
        printfn "No username, please enter a username"
        System.Environment.Exit(1)
        None
    | _ -> 
        Some username

let parseCommand commandLineArgs =
    let term = commandLineArgs |> Array.tryItem 1
    match term with
    | None -> 
        PrintHelp
    | Some command ->
        match command with
        | "timeline" -> 
            let username = collectUsername() |> Option.get
            TimelineAnalysis(accessToken, username)
        | "luceneindex" ->
            let username = collectUsername() |> Option.get
            PostLuceneIndex(accessToken, username)
        | "lucenesearch" ->
            let termValue = commandLineArgs |> Array.tryItem 2
            match termValue with
            | None -> 
                PrintHelpWithError "No search term provided"
            | Some termValue -> 
                PostLuceneSearch termValue
        | "search" ->
            let termValue = commandLineArgs |> Array.tryItem 2
            match termValue with
            | None -> 
                PrintHelpWithError "No search term provided"
            | Some termValue ->
                let username = collectUsername() |> Option.get
                PostSearch(accessToken, username, termValue)
        | _ -> PrintHelp

let previousTitle = System.Console.Title
System.Console.Title <- "Mastodon Playground"
let commandLineArgs = System.Environment.GetCommandLineArgs()

commandLineArgs |> Array.contains "--verbose" |> MastodonPlayground.Configuration.setVerbose
commandLineArgs |> Array.contains "--cached" |> MastodonPlayground.Configuration.setUseCache

let command = parseCommand commandLineArgs
match command with
| PrintHelp -> 
    printUsage()
| PrintHelpWithError error ->
    printError error
    printUsage()
| TimelineAnalysis(accessToken, username) ->
    MastodonPlayground.TimelineAnalysis.run accessToken username  
| PostSearch(accessToken, username, termValue) ->
    MastodonPlayground.PostSearch.run accessToken username termValue
| PostLuceneIndex(accessToken, username) ->
    MastodonPlayground.LuceneExperiment.indexPosts accessToken username
| PostLuceneSearch termValue ->
    MastodonPlayground.LuceneExperiment.searchPosts termValue

System.Console.Title <- previousTitle