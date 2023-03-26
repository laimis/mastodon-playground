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

// the command line parameter will provide the search term
let term = System.Environment.GetCommandLineArgs() |> Array.tryItem 1

match term with
| None -> 
    MastodonPlayground.TimelineAnalysis.run accessToken username
    
| Some termValue ->
    MastodonPlayground.PostSearch.run accessToken username termValue