open Microsoft.Extensions.Configuration

type Something = { Name: string; Value: int }

type AccessToken = string
type Username = string
type SearchTerm = string

type Command =
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

System.Console.WriteLine("Enter username:")
let username = System.Console.ReadLine().Trim()

match username with
| "" -> 
    printfn "No username, please enter a username"
    System.Environment.Exit(1)
| _ -> // good to go

let command = 
    let term = System.Environment.GetCommandLineArgs() |> Array.tryItem 1
    match term with
    | None -> 
        TimelineAnalysis(accessToken, username)
    | Some termValue ->
        PostSearch(accessToken, username, termValue)

match command with
| TimelineAnalysis(accessToken, username) ->
    MastodonPlayground.TimelineAnalysis.run accessToken username  
| PostSearch(accessToken, username, termValue) ->
    MastodonPlayground.PostSearch.run accessToken username termValue