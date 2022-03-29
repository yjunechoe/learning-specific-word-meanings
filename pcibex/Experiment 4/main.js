PennController.ResetPrefix(null) 
DebugOff()
var showProgressBar = false;

PennController.Sequence("intro", "counter", "beginning", "experiment", "end", SendResults(), "bye")

SetCounter("counter", "inc", 1);

newTrial( "intro" ,
    newText("<p style=font-size:18px;>Welcome to our study!</p>" +
            "<p style=font-size:18px;>Press Enter to go full screen and begin!:</p>")
        .center()
        .print()
    ,
    newKey("Enter","ENTER")
        .wait()
    ,
    fullscreen()

).setOption("hideProgressBar",true)

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

Template("01_trial_templates_v2.csv", row => 
    // learn-phase goes here before test-phase
    newTrial("experiment",
        // init learn and test content
        newSallyCanvas("learn-phase", row.learn_set)
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
        newButton("Continue")
            .center()
            .cssContainer({"position": "relative"})
            .css({
                "position" : "absolute",
                "top" : "-175px",
                "transform": "translateX(-50%)",
                "z-index": "2",
                "font-size": "24px",
                "padding": "10px"
            })
        ,
        newSelectionArray("test-phase", row.test_set)
            .print()
            .center()
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
            .sallySay(resolveSallyMessage(row.label1, true))
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
                        .sallySay(resolveSallyMessage(row.label2, false))
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
        getSelectionArray("test-phase")
            .show()
        ,
        getButton("Continue")
            .print()
            .wait()
    )
    .log("group", row.group)
    .log("labelled", row.labelled)
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
            "<p style=font-size:18px;><a href='https://app.prolific.co/submissions/complete?cc=42CB45E7'>Click here to confirm my submission on Prolific</a>.</p>")
        .center()
        .print()
        .wait()
).setOption("hideProgressBar",true)
