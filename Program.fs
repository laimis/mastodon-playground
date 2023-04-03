open Microsoft.Extensions.Configuration

type Something = { Name: string; Value: int }

type AccessToken = string
type Username = string
type SearchTerm = string

type Command =
    | PrintHelp
    | TimelineAnalysis of AccessToken * Username
    | PostSearch of AccessToken * Username * SearchTerm

let builder = new ConfigurationBuilder()
builder.AddUserSecrets<Something>() |> ignore // had to create this type to get it to work
let config = builder.Build()
let accessToken = config["accesstoken"]

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


let command = 
    let term = System.Environment.GetCommandLineArgs() |> Array.tryItem 1
    match term with
    | None -> 
        PrintHelp
    | Some command ->
        match command with
        | "timeline" -> 
            let username = collectUsername() |> Option.get
            TimelineAnalysis(accessToken, username)
        | "search" ->
            let termValue = System.Environment.GetCommandLineArgs() |> Array.tryItem 2
            match termValue with
            | None -> PrintHelp
            | Some termValue ->
                let username = collectUsername() |> Option.get
                PostSearch(accessToken, username, termValue)
        | _ -> PrintHelp

match command with
| PrintHelp -> 
    printfn "Usage: dotnet run <command> <search term>"
    printfn "Commands:"
    printfn "  timeline - Analyze the timeline of a user"
    printfn "  search - Search for a term in a user's posts"
| TimelineAnalysis(accessToken, username) ->
    MastodonPlayground.TimelineAnalysis.run accessToken username  
| PostSearch(accessToken, username, termValue) ->
    MastodonPlayground.PostSearch.run accessToken username termValue