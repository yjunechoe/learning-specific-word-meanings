PennController.ResetPrefix(null)
DebugOff()
var showProgressBar = false;

PennController.Sequence("intro", "counter", "beginning", "experiment", "end", SendResults(), "bye")

SetCounter("counter", "inc", 1);

/*Intro for use with Sona
                Participants see a screen and are told to press enter to go full screen and begin the study.
                In the background, the URL parameter "survey_code" is set as the global variable "ID".
                This trial logs the ID number to the results. Be sure to add the ".log("ID", getVar("ID")"  line
                                at the end of every trial or trial template.*/
newTrial( "intro" ,
    newText("<p style=font-size:18px;>Welcome to our study!</p>" +
            "<p style=font-size:18px;>Press Enter to go full screen and begin!:</p>")
        .center()
        .print()
    ,
    // newVar("ID")
    //    .global()
    //    .set(GetURLParameter("survey_code"))
    //,
    newKey("Enter","ENTER")
        .wait()
    ,
    fullscreen()

).setOption("hideProgressBar",true)
//.log("ID", getVar("ID"))

PennController("beginning",
    defaultSallyCanvas
        .css("height", "550px")
        .css("width", "1100px")
        .css("position", "relative")
        .css("margin-top", "-50px")
        .css("margin-bottom", "2rem")
        .css("outline", "none")
        .showSpeechBubble()
        .print()
    ,
    defaultButton
        .cssContainer("margin-top", "2rem")
        .cssContainer("margin-bottom", "1rem")
        .center()
        .print()
        .wait()
        .remove()
    ,
    newSallyCanvas("beginning-message")
        .sallySay("Hi there!<br>My name is Sally!")
    ,
    newButton("Next-1", "Next")
    ,
    getSallyCanvas("beginning-message")
        .sallySay("I'd like to teach you some words<br>from my native language.")
    ,
    newButton("Next-2", "Next")
    ,
    getSallyCanvas("beginning-message")
        .sallySay("Please pay attention because I will be<br>asking you questions about these words later!")
    ,
    newButton("Next-3", "Next")
    ,
    getSallyCanvas("beginning-message")
        .sallySay('Are you ready?<br>Click the "Begin" button when you are ready!')
    ,
    newButton("Begin")
)

Template("01_trial_templates.csv", row => 
    // learn-phase goes here before test-phase
    newTrial("experiment",
        // init learn and test content
        newSallyCanvas("learn-phase", row.learn_set, row.order)
            .css("height", "550px")
            .css("width", "1100px")
            .css("position", "relative")
            .css("margin-bottom", "2rem")
            .print()
        ,
        newSallyMessageBar("directions", row[row.target])
            .cssContainer("display", "block")
            .print()
            .hidden()
        ,
        newSelectionGrid("test-phase", row.test_set)
            .print()
            .hide()
            .log()
        ,
        // show speech bubble above sally after 1 sec
        newTimer("show-speech-bubble", 1000)
            .start()
            .wait()
        ,
        getSallyCanvas("learn-phase")
            .showSpeechBubble()
            .sallySay("Okay!<br>Pay attention now!")
        ,
        // teach left referent
        newTimer("hold-speech-bubble", 3000)
            .start()
            .wait()
        ,
        getSallyCanvas("learn-phase")
            .showFirst()
            .sallySay(resolveName(row.label1, row.order == "3-1", true))
        ,
        // teach referent(s)
        newTimer("learn-referent", 7000)
        ,
        getSallyCanvas("learn-phase")
            .test.noContrast()
                .failure(
                    getTimer("learn-referent")
                        .start()
                        .wait()
                    ,
                    getSallyCanvas("learn-phase")
                        .hideAll()
                        .showSecond()
                        .sallySay(resolveName(row.label2, row.order == "1-3", false))
                )
        ,
        getTimer("learn-referent")
            .start()
            .wait()
        ,
        // hide all taught referents
        getSallyCanvas("learn-phase")
            .hideAll()
            .sallyStill()
            .hideSpeechBubble()
        ,
        // move to test phase after 3 sec
        newTimer("show-selection-grid", 1000)
            .start()
            .wait()
        ,
        getSallyCanvas("learn-phase")
            .remove()
        ,
        getSallyMessageBar("directions")
            .visible()
        ,
        getSelectionGrid("test-phase")
            .show()
        ,
        newButton("Continue")
            .cssContainer("margin", "1rem")
            .print()
            .center()
            .wait(getSelectionGrid("test-phase").test.selectAny())
    )
    .log("group", row.group)
    .log("type", row.type)
    .log("order", row.order)
    .log("target", row.target)
    .log("item", row.domain)
)

PennController("end",
    newSallyCanvas("end-message")
        .css("height", "550px")
        .css("width", "1100px")
        .css("position", "relative")
        .css("margin-top", "-150px")
        .css("margin-bottom", "2rem")
        .css("outline", "none")
        .showSpeechBubble()
        .sallySay("That was fun!<br>Thanks for playing with me!")
        .print()
    ,
    newButton("Finish")
        .cssContainer("margin-top", "2rem")
        .cssContainer("margin-bottom", "1rem")
        .center()
        .print()
        .wait()
).log("window_size", `${window.innerWidth}x${window.innerHeight}` )

/* This is the final trial for a Sona study. Participants will be instructed to click the redirect link
                                (you can find this on your Sona experiment page) to return to Sona. Sona will recognize their
                                survey code and automatically grant credit.*/
newTrial( "bye" ,
    newText("<p style=font-size:18px;>Your results have been saved, but you need to validate your participation below.</p>" +
            "<p style=font-size:18px;><a href=''>Click here to confirm my submission on Prolific</a>.</p>")
        .center()
        .print()
        .wait()
).setOption("hideProgressBar",true)
