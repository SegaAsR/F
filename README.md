let lastFmScore = 
    memo (
        fun ((v1 : string), (v2 : string)) ->
            let APIKey = "..." //здесь нужно вставить API-key
            let head = "http://ws.audioscrobbler.com/2.0/\
                       ?method=tasteometer.compare"
            let uri = sprintf
                    "%s&type1=user&type2=user&value1=%s\
                    &value2=%s&api_key=%s" 
                    head v1.[26..] v2.[26..] APIKey
            use stream = WebRequest.Create(uri).GetResponse()
                         .GetResponseStream()
            use reader = new StreamReader(stream, Encoding.Default)
            let result = XElement.Parse(reader.ReadToEnd())
            let score = result.Descendants(!!"score").First().Value 
                        |> float
            score
    )

let lastFmScore = 
    memo (
        fun ((v1 : string), (v2 : string)) ->
            let APIKey = "..." //здесь нужно вставить API-key
            let head = "http://ws.audioscrobbler.com/2.0/\
                       ?method=tasteometer.compare"
            let uri = sprintf
                    "%s&type1=user&type2=user&value1=%s\
                    &value2=%s&api_key=%s" 
                    head v1.[26..] v2.[26..] APIKey
            use stream = WebRequest.Create(uri).GetResponse()
                         .GetResponseStream()
            use reader = new StreamReader(stream, Encoding.Default)
            let result = XElement.Parse(reader.ReadToEnd())
            let score = result.Descendants(!!"score").First().Value 
                        |> float
            score
    )

//список игнорируемых служебных полей
let excluded = new Set<_>(["Id"; "Friends"])
 
//функция, сравнивающая все сравнимые признаки двух пользователей
//metrics — словарь семиметрик для разных признаков: 
//IDictionary<string, (string -> string -> float)>
//excluded — список служебных полей, которые надо игнорировать
//silent — отключить отладочную печать
let featureScores (metrics : IDict<_, _>) (excluded : Set<_>) silent 
    (user1 : IDict<_, _>) (user2 : IDict<_, _>) = 
    dict [
        for feature in user1.Keys do
            if not <| excluded.Contains feature 
               && user1.ContainsKey feature
               && user2.ContainsKey feature then                
                let score = 
	            (if metrics.ContainsKey feature 
                        then metrics.[feature] 
                        else metrics.["default"]) 
                        (user1.[feature], user2.[feature])        
                //если не включён "тихий" режим, отладочная печать
                if not silent then 
                    printfn "Key: [%s]\nScore: %f" feature score
                yield feature, score
    ]

//функция с семиметриками и исключениями по умолчанию
let defFeatureScores u1 u2 = featureScores metrics excluded true u1 u2

//посчитать расстояние между пользователями
//scoresInteractions : IDict<_, _> -> IDict<_, _> — изменяет словарь оценок 
//похожести признаков, учитывая взаимоотношения между ними
let compareUsers scoresInteractions u1 u2 = 
    let (scores : IDict<_, _> ) = defFeatureScores u1 u2 
                                  |> scoresInteractions 
    (Seq.sum scores.Values) / (float scores.Count)
    
//если присутствуют оценки и для Last.fm и для Любимой музыки, 
//оставляем максимальную из них
let scoreInteractionLastFMMusic (scores : IDict<_, _>) =
    if scores.ContainsKey "Last.fm" 
       && scores.ContainsKey "Любимая музыка" then
       let minScoreKey = 
           if scores.["Last.fm"] < scores.["Любимая музыка"] 
              then "Last.fm" else "Любимая музыка"
       dict [
            for key in scores.Keys do
                if key <> minScoreKey then
                    yield key, scores.[key]
       ]
    else scores
 
let defCompareUsers u1 u2 = compareUsers scoreInteractionLastFMMusic 
                            u1 u2
 
//расстояние между двумя пользователями = (1 - похожесть пользователей)
let distance u1 u2 = 1. - defCompareUsers u1 u2

//рекомендует пользователя, если он ближе, чем среднее расстояние друзей     
//userId — ключ пользователя user
//user — словарь признаков пользователя, для которого делаем рекомендации
//userFriends — друзья пользователя user
//allUsers — пользователи, из которых делаем рекомендации
let simpleRecommender alpha userId (user : IDict<_, _>) 
    (userFriends : IDict<_, _>) (allUsers : IDict<_, _>) =
    
    //расстояния до друзей пользователя user с id userId
    let userFriendsDistances = 
        [| 
            for friend in userFriends.Keys do
                yield distance user userFriends.[friend]
        |]
        |> Array.filter (fun num -> not <| Double.IsNaN num)
    //среднее расстояние до друзей пользователя user
    let meanFriendDistance = 
         (Array.sum userFriendsDistances) 
          / (alpha * (float userFriendsDistances.Length))
 
    //массив рекомендаций
    [|
        for target in allUsers.Keys do
            if target <> userId then
                let dist = distance allUsers.[target] user
                if dist <= meanFriendDistance then 
                    yield target//, dist
    |]

let politicsScore (v1 : string) (v2 : string) =
    let totalitarian = //мера терпимости к тоталитарности
        dict [
            "Либертарианские", 0.0; "Либеральные", 0.1; 
            "Социалистичеcкие", 0.3; "Индифферентные", 0.5
            "Умеренные", 0.5; "Коммунистические", 0.7;
            "Консервативные", 0.7; "Монархические", 1.0
            "Ультраконсервативные", 1.0
        ]
    let social = //желание социальных гарантий от гос-ва
        dict [
            "Либертарианские", 0.0; "Либеральные", 0.2;
            "Социалистичеcкие", 0.8; "Индифферентные", 0.5
            "Умеренные", 0.5; "Коммунистические", 1.0;
            "Консервативные", 0.6; "Монархические", 0.6
            "Ультраконсервативные", 0.6
        ]

//если взгляды представлены в предопределённых вариантах
    if totalitarian.ContainsKey v1 && totalitarian.ContainsKey v2 then
        let totalitarianDistance = Math.Abs(totalitarian.[v1] 
                                   - totalitarian.[v2])
        let socialDistance = Math.Abs(social.[v1] - social.[v2])
        //конечная оценка = (1. - среднее арифметическое)
        let score = 1. - (totalitarianDistance + socialDistance) / 2.
        score //ближе к 1 — похожи, ближе к 0 — не похожи
    //иначе использовать обычную текстовую семиметрику
    else generalTextScore v1 v2

let outlookScore (v1 : string) (v2 : string) =
    let humanist = //мера гуманистичности
        dict [
            "Атеист", 1.0; "Атеизм", 1.0; "Агностик", 1.0;
            "Агностицизм", 1.0; "Светский гуманизм", 1.0; 
		   "Конфуцианство", 0.8; "Буддизм", 0.8; "Протестантизм", 0.5;
  	   "Ислам", 0.0; "Католицизм", 0.0; "Православие", 0.0;
            "Иудаизм", 0.0;
        ]
    let v1', v2' = v1.ToLower().Trim(), v2.ToLower().Trim()
    if humanist.ContainsKey v1' && humanist.ContainsKey v2' then 
1.0 - Math.Abs(humanist.[v1'] - humanist.[v2'])
    else
        generalTextScore v1' v2'

let attitudeScore (v1 : string) (v2 : string) =
    let attitude =
        dict [
            "Резко негативное", 0.0; "Негативное", 0.2; 
            "Компромиссное", 0.5; "Нейтральное", 0.5;
            "Положительное", 1.0;
        ]    
    if attitude.ContainsKey v1 && attitude.ContainsKey v2 then 
        1. - Math.Abs(attitude.[v1] - attitude.[v2])
    else
        generalTextScore v1 v2     dict [ 
        "default", generalTextScore
        "Last.fm", lastFmScore
        "Полит. предпочтения", politicsScore
        "Мировоззрение", outlookScore
        "Отн. к курению", attitudeScore
        "Отн. к алкоголю", attitudeScore ]

//средние похожести с друзьями для всех признаков - веса
//не нормализуем, мы не знаем, сколько признаков будут 
//в пересечении множеств для пользователя и его друзей
let avgScores = 
    //сравниваем пользователя со всеми его друзьями
    let friendsScores = 
        dict [
            for friend in lakretFriends.Keys do            
                let scores = defFeatureScores lakret 
                                              lakretFriends.[friend]
                yield friend, scores
        ]
 
    dict [
        for feature in lakret.Keys do
            if not <| excluded.Contains feature then
                let values = 
                    [
                        for friend in friendsScores.Keys do
                            if friendsScores.[friend].ContainsKey 
                               feature then
                                yield friendsScores.[friend].[feature]
                    ]
                yield feature, (List.sum values) / (float values.Length)
    ]

let totalScore' (weights : IDict<_, _>) (scores : IDict<_, _>) =
    let mutable total = 0.0
    let mutable normalizer = 0.0
    for feature in scores.Keys do
        if weights.ContainsKey feature then
            total <- total + weights.[feature] * scores.[feature]
            normalizer <- normalizer + weights.[feature]
    total / normalizer
 
let compareUsers' scoresInteractions u1 u2 = 
    let scores = defFeatureScores u1 u2 |> scoresInteractions
    totalScore' avgScores scores
 
let defCompareUsers' u1 u2 = 
    compareUsers' scoreInteractionLastFMMusic u1 u2
let distance' u1 u2 = 1. - defCompareUsers' u1 u2
 
let recommender alpha  =
    //расстояния до друзей пользователя "lakret"
    let lakretFriendsDistances = 
        [| 
            for friend in lakretFriends.Keys do
                yield distance' lakret lakretFriends.[friend]
        |]
        |> Array.filter (fun num -> not <| Double.IsNaN num)
 
    let meanFriendDistance = 
        (Array.sum lakretFriendsDistances) 
        / (alpha * (float lakretFriendsDistances.Length))
 
    [|
        for user in mainUsers.Keys do
            if user <> "lakret" then  
                if distance' mainUsers.[user] mainUsers.["lakret"] 
                   <= meanFriendDistance then 
                    yield user //, score
    |]

let scores user (users : IDict<_, _>) = 
    let dict = new Dictionary<_, List<_>>()
    //вспомогательная функция, которая добавляет оценки 
    //для текущего пользователя
    //к словарю списков оценок по всем признакам
    let concat (dict : Dictionary<_, List<_>>) (ithUser : IDict<_, _>) =
        for key in ithUser.Keys do
            if dict.ContainsKey key then
                dict.[key].Add(ithUser.[key])                
            else 
                dict.Add(key, new List<_>())
                dict.[key].Add(ithUser.[key])
        dict                
    
    //массив словарей с оценками похожести для всех
    let scoresArray = 
        [|
            for target in users.Keys do            
                let score = defFeatureScores user users.[target]
                yield score //IDict<feature name, score value>
        |]
    //объединяем все эти словари оценок в один словарь со списками оценок
    //в качестве значений
    Array.fold concat dict scoresArray

let scoreStatistics (scores : IDict<_, List<float>>) = 
    dict [ 
        for feature in scores.Keys do
            let mean = scores.[feature].Mean()
            let variance = scores.[feature].PopulationVariance()
            let distribution = Normal.WithMeanVariance(mean, variance)
            let P = distribution.Density //частичное применение
            //вернёт функцию: значение -> log вер.
            yield feature, P >> Math.Log10 
    ]

let naiveBayesRecommender (user : IDict<_, _>) (friends : IDict<_, _>) (notFriends : IDict<_, _>) = 
    let friendsStatistics = scoreStatistics <| scores user friends
    let notFriendsStatistics = scoreStatistics <| scores user notFriends
    //вспомогательная функция, считает вероятность P(C|X1,...,Xn)
    //statistics — логарифмы плотностей вероятностей 
    //для значений разных признаков
    //scores — признаки для исследуемого кандидата
    let prob (statistics : IDict<_, _>) (scores : IDict<_, _>) = 
        [| for feature in scores.Keys -> statistics.[feature]
                                             scores.[feature] |] 
        |> Array.filter (not << Double.IsNaN) |> Array.sum

    [|
        for target in notFriends.Keys do
            let scores = defFeatureScores user notFriends.[target]
            let pFriend = prob friendsStatistics scores
            let pNotFriend = prob notFriendsStatistics scores
            if pFriend >= pNotFriend then yield target
    |]

let naiveBayesRecommendations = 
        naiveBayesRecommender lakret lakretFriends mainUsers
 
 //пользователи, которые не являются друзьями, 
//и для которых ground truth == 1
let groundTruthYes =
    new Set<_>( 
        [
            for user in mainUsers.Keys do
                if (not <| lakretFriends.ContainsKey user) 
                   && (user <> "lakret") then
                    if mainUsers.[user].["Ground Truth Lakret"] = "1" 
                    then yield user
        ])

//Fbeta score
let Fbeta beta (recommendations : _ []) = 
    //рекомендован и ground truth = 1
    let truePositives = float (Array.filter 
        (fun key -> groundTruthYes.Contains key) recommendations).Length
    let precision = truePositives / (float <| recommendations.Length)
    let actualPositives = float <| groundTruthYes.Count
    let recall =  truePositives / actualPositives
    let score = ((1. + beta ** 2.) * precision * recall) 
                 / ((beta ** 2.) * precision + recall)
    printfn "TP: %i, FP: %i" (int truePositives) 
            (recommendations.Length - (int truePositives))
    printfn "Precision: %f, Recall: %f" precision recall
    printfn "Fβ score (β = %.2f): %f" beta score
    score
 
//F1 score
let F1 = Fbeta 1.
//больший акцент на точности, чем на полноте
let Fhalf = Fbeta 0.5 

//memoization
let memo f = 
    let dict = new Dictionary<_,_>()
    fun x ->
        if not <| dict.ContainsKey(x) then
            dict.Add(x, f x)
        dict.[x]
 
//usage example:
//#nowarn "40"
//let rec fib = 
//    memo (
//        function 
//        | 0 | 1 -> 1
//        | x -> fib (x - 1) + fib (x - 2)
//    )
 
//mutable dictionary
let mutableDict (pairs : (_*_) seq) = 
    let dict = new Dictionary<_, _>()
    for item in pairs do
        dict.Add(fst item, snd item)
    dict
 
//converts string to XName
let (!!) = XName.op_Implicit
 
//сокращённое название типа
type IDict<'a, 'b> = IDictionary<'a, 'b>
 
//константа 
let π = Math.PI
 
//maybe monad
type maybeBuilder () = 
    member this.Bind (x, cont) = 
        match x with
        | Some v -> cont v
        | None -> None
    member this.Return x = Some x
    //member this.Delay f = f()
 
let maybe = maybeBuilder()


//для получения cookies надо авторизоваться
let main = new Form()
main.Width <- 600
main.Height <- 400
let browser = new WebBrowser()
browser.Dock <- DockStyle.Fill
main.Controls.Add(browser)
browser.Navigate(@"vk.com/lakret")
main.Show()
 
//Парсит строку с cookies от WebBrowser и возвращает CookieContainer
let getCookieContainer (browser: WebBrowser) domain = 
    let container = new CookieContainer()
    for cookie in browser.Document.Cookie.Split(';') do        
        let elements = cookie.Split('=')
        let name = elements.[0]
        let value = if elements.Length > 1 then cookie.Substring(name.Length + 1) else ""
        let path = "/"
        container.Add(new Cookie(name.Trim(), value.Trim(), path, domain))    
    container
let cookies = getCookieContainer browser ".vk.com"
 
//Сериализация CookieContainer'a
let serializeCookieContainer cookies =
    use stream = File.Open(@"C:\cookiesContainer1.bin", FileMode.Create) in
    let formatter = new BinaryFormatter()
    formatter.Serialize(stream, cookies)
    stream.Close()
 
serializeCookieContainer cookies
 
//Десериализация CookieContainer'a
let deserializeCookieContainer () = 
    use stream = File.Open(@"C:\cookiesContainer1.bin", FileMode.Open) in
    let formatter = new BinaryFormatter()
    formatter.Deserialize(stream) :?> CookieContainer
 
let cookies' = deserializeCookieContainer()
let prefix = @"http://vk.com" //prefix for links
 
//Асинхронно скачать набор страниц
let getPages urls cookies = 
    let getPageAsync (url : string) = 
        async {            
            let request = WebRequest.Create(url) :?> HttpWebRequest
            request.CookieContainer <- cookies
            let! response = request.AsyncGetResponse()
            use stream = response.GetResponseStream()
            use reader = new StreamReader(stream, Encoding.Default)
            return! reader.AsyncReadToEnd()
        }
    Продолжение прил. Б
    Seq.map getPageAsync urls
    |> Async.Parallel
    |> Async.RunSynchronously

//get document node from string with Html
let getDocumentNode page = 
    let doc = new HtmlDocument()
    doc.LoadHtml(page)
    doc.DocumentNode
 
//get user profile from document node
let getProfile (doc : HtmlNode) = 
    try
        doc.SelectNodes(@"//div[@id = 'profile_full_info']").[0] 
    with
    | _ -> null
 
//Link to user's friends
let getFriends (doc : HtmlNode) prefix cookies = 
    let friendsPageLink = 
        doc.SelectNodes(@"//div[@id = 'profile_friends'][1]/a[1]/@href[1]").[0]
           .Attributes.Where(fun (attr : HtmlAttribute) -> attr.Name = "href")
           .First().Value    
    let friendsPage = (getPages' [prefix + friendsPageLink] cookies).[0]
    let friendsDoc = getDocumentNode friendsPage
    let allFriends = 
        friendsDoc.SelectNodes(@"//div[@id = 'friends']/div[@id = 'main_class']//div[@id = 'list_content']
                                 /*/div[@class = 'user_block clear_fix' or @class = 'user_block user_block_first clear_fix']
                                 /div[@class = 'info fl_l']/div[@class = 'friends_field']/a").ToArray()
        |> Array.map (fun (node : HtmlNode) -> prefix + node.Attributes.[0].Value)        
    let friendsPages = getPages' allFriends cookies
    Array.zip allFriends friendsPages
 
let getFriendsDefault doc = getFriends doc prefix cookies'
 
//extract user info from profile
let parseProfile (profile : HtmlNode) =
    //вспомогательная функция, извлекающая все пары ключ-значение из потомков данного узла
    let extractItems (node : HtmlNode) =
        mutableDict [
            for item in node.ChildNodes do            
                if item.ChildNodes.Count >= 4 then 
                    let key = item.ChildNodes.[1].InnerText
                    let value = item.ChildNodes.[3].InnerText
                    if key.Length > 0 && key.[..(key.Length-2)] <> "&nbsp" && value.Length > 0 then
                        yield key.[..(key.Length-2)], value
        ]
  
 //"Жизненная позиция" — следующий элемент за заголовком
 // "<h4> ... <b>Жизненная позиция</b></h4>"
    let position = profile.SelectNodes(@"div[preceding-sibling::h4[b/text() = 'Жизненная позиция']]")
    //если есть блок "Жизненная позиция"
    let result = if position <> null then extractItems position.[0] else mutableDict [] //ключи-значения позиции
    //"Личная информация" — следующий div-сиблинг, если есть "Жизненная позиция"
    //let personalInfo = position.SelectNodes(@"following-sibling::div").[0]
    let personalInfo = profile.SelectNodes(@"div[preceding-sibling::h4[b/text() = 'Личная информация']]")
    let personalInfoItems = if personalInfo <> null then extractItems personalInfo.[0] else mutableDict []
    for item in personalInfoItems do result.Add(item.Key, item.Value)
    result
 
let xmlRepresentation link (profile : Dictionary<string, string>) =
    //profile
    let xml = new XElement(!!"user")
    xml.Add(new XAttribute(!!"uri", link))
    for pair in profile do        
        let interest = new XElement(!!"interest")
        interest.Add(new XElement(!!"key", pair.Key))
        interest.Add(new XElement(!!"value", pair.Value))
        xml.Add interest
    xml
 
let saveXmlRepresentation folder offset (xml : XElement)=
    let uri = xml.Attributes(!!"uri").First().Value
    let filename = folder + uri.[offset..] + ".xml"
    xml.Save(filename : string)
 
//offset = length of "http://vk.com/" = 14
let defaultSaveXml xml = saveXmlRepresentation 
let defaultSaveXmlMain xml = saveXmlRepresentation 
 
let processUserAndItsFriends cookies url = 
    printfn "Processing [%s]..." url
    let userPage = (getPages' [url] cookies).[0]
    let doc = getDocumentNode userPage
    let profileDiv = getProfile doc
    let lastFM = Regex.Match(profileDiv.InnerHtml, 
    let profile = parseProfile profileDiv
    if lastFM <> "" then
        profile.Add("Last.fm", lastFM)
    let friends = getFriendsDefault doc
    let parseFriendsProfiles =
       [|            
            for (link, page) in friends do             
                   Окончание прил. Б

		 printfn "Processing friends of [%s]: [%s]" url link
                let profile = getDocumentNode >> getProfile <| page
                if profile <> null then
                    let parsedProfile = parseProfile profile
                    yield link, parsedProfile
        |]
    profile.Add("Friends", Array.fold (fun list friend -> list + fst friend + ", ") "" parseFriendsProfiles)
    xmlRepresentation url profile |> defaultSaveXmlMain
    Array.Parallel.iter (fun (link, profile) -> xmlRepresentation link profile |> defaultSaveXml) parseFriendsProfiles
    printfn "[%s] and his/her friends saved." url
 
let processAll () = 
    let mainUsers = File.ReadAllLines 
    Array.iter (processUserAndItsFriends cookies') mainUsers
 
//processAll ()
 
let lFriendsAll() = 
    let allFriends = 
        File.ReadAllLines 
        |> Array.map (fun str -> prefix + str)
        //prefix
    let friendsPages = getPages' allFriends cookies'
    let friends = Array.zip allFriends friendsPages
    for (link, page) in friends do
        printfn "Processing friends: [%s]" link
        let profileDiv = getDocumentNode >> getProfile <| page
        if profileDiv <> null then
            let lastFM = Regex.Match(profileDiv.InnerHtml,    
            let parsedProfile = parseProfile profileDiv
            if lastFM <> "" then
                parsedProfile.Add("Last.fm", lastFM)
            xmlRepresenta-tion link parsedProfile |> saveXmlRepresentation

