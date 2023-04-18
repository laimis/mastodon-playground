namespace MastodonPlayground

module Configuration =

    let mutable isVerbose = false
    let mutable doUseCache = false

    let setVerbose verbose =
        isVerbose <- verbose

    let setUseCache useCache =
        doUseCache <- useCache