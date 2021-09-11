PennController.ResetPrefix(null)

PennController.Sequence("consent", randomize("experiment"))

PennController( "consent" ,
    defaultText
        .print()
    ,
    newText("<h2><b>CONSENT TO PARTICIPATE IN RESEARCH</b></h2>")
        .settings.center()
    ,
    newButton("I Agree")
        .settings.center()
        .print()
        .wait()
)

Template("choice_set_df.csv", row => 
    // learn-phase goes here before test-phase
    newTrial("experiment",
        // init learn and test content
        newSallyCanvas("learn-phase")
            .css("height", "550px")
            .css("width", "1100px")
            .css("position", "relative")
            .print()
        ,
        newSelectionGrid("test-phase", row.choices)
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
        ,
        // move to test phase after 3 sec
        newTimer("show-selection-grid", 3000)
            .start()
            .wait()
        ,
        getSelectionGrid("test-phase")
            .show()
        ,
        newButton("Continue")
            .print()
            .center()
            .wait(getSelectionGrid("test-phase").test.selectAny())
    )
)
