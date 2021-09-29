PennController.ResetPrefix(null)
DebugOff()
var showProgressBar = false;

PennController.Sequence("consent", "counter", "intro", "beginning", "experiment", "end")

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

PennController("intro",
    defaultText
        .css("font-style", "italic")
        .center()
        .print()
        .cssContainer("margin-top", "1rem")
    ,
    newImage("sally", "https://raw.githubusercontent.com/yjunechoe/learning-specific-word-meanings/main/image_stimuli/Sally/pointer-none.png")
        .cssContainer("margin-top", "5rem")
        .cssContainer("margin-bottom", "2rem")
        .print()
        .center()
    ,
    newText("This is Sally.")
    ,
    newText("Sally would like to teach you some words in her native language.")
    ,
    newText("Please pay attention to what Sally says, because she will be asking you questions about them.")
    ,
    newButton("Begin")
        .cssContainer("margin-top", "3rem")
        .cssContainer("margin-bottom", "5rem")
        .center()
        .print()
        .wait()
)

PennController("beginning",
    newSallyCanvas("beginning-message")
        .css("height", "550px")
        .css("width", "1100px")
        .css("position", "relative")
        .css("margin-top", "-150px")
        .css("margin-bottom", "2rem")
        .css("outline", "none")
        .showSpeechBubble()
        .sallySay("Hi! My name is Sally!<br>I'd like to teach you some words from my native language. Please pay attention because I will be asking you questions about these words later!")
        .print()
    ,
    newButton("Begin")
        .cssContainer("margin-top", "2rem")
        .cssContainer("margin-bottom", "1rem")
        .center()
        .print()
        .wait()
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
            .sallySay("Okay!<br>Are you ready?")
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
