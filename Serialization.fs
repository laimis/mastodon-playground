namespace MastodonPlayground

module Serialization =

    let internal serialize content =
        // need to use newtonsoft because the project uses newtonsoft attributes to change property names
        Newtonsoft.Json.JsonConvert.SerializeObject(content)

    let internal deserialize<'a> (content:string) =
        // need to use newtonsoft because the project uses newtonsoft attributes to change property names
        Newtonsoft.Json.JsonConvert.DeserializeObject<'a>(content)