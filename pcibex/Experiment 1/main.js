PennController.ResetPrefix(null)
DebugOff()
var showProgressBar = false;

PennController.Sequence("consent", "counter", "beginning", "experiment", "end")

SetCounter("counter", "inc", 1);

PennController( "consent" ,
    defaultText
        .print()
    ,
    newText("<h2><b>CONSENT TO PARTICIPATE IN RESEARCH</b></h2>")
        .print()
        .center()
    ,
    newButton("I Agree")
        .center()
        .print()
        .wait()
)

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
        newSallyCanvas("learn-phase", row.learn_set)
            .css("height", "550px")
            .css("width", "1100px")
            .css("position", "relative")
            .css("margin-bottom", "2rem")
            .print()
        ,
        newSallyMessageBar("directions", row.label1)
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
            .sallySay("Look, this is a " + row.label1 + "!<br>Do you see the " + row.label1 + "?")
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
                        .sallySay("And look, this is a " + row.label2 + "!<br>Do you see the " + row.label2 + "?")
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
    .log("condition", row.condition)
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
)
