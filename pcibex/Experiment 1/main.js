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
            .sallySay("Are you ready?")
        ,
        // teach left referent
        newTimer("show-first", 3000)
            .start()
            .wait()
        ,
        getSallyCanvas("learn-phase")
            .showFirst()
            .sallySay("Look, this is a " + row.label1 + "!<br>Do you see the " + row.label1 + "?")
        ,
        // teach right referent
        newTimer("show-second", 5000)
        ,
        getSallyCanvas("learn-phase")
            .test.noContrast()
                .failure(
                    getTimer("show-second")
                        .start()
                        .wait()
                    ,
                    getSallyCanvas("learn-phase")
                        .showSecond()
                        .sallySay("And look, this is a " + row.label2 + "! Do you see the " + row.label2 + "?")
                )
        ,
        // hide all taught referents
        newTimer("hide-all", 5000)
            .start()
            .wait()
        ,
        getSallyCanvas("learn-phase")
            .sallyStill()
            .hideSpeechBubble()
        ,
        // move to test phase after 3 sec
        newTimer("show-selection-grid", 3000)
            .start()
            .wait()
        ,
        getSallyCanvas("learn-phase")
            .hideAll()
            .sallySay("Do you see any other " + row.label1 + "s below?")
            .showSpeechBubble()
        ,
        getSelectionGrid("test-phase")
            .show()
        ,
        newButton("Continue")
            .cssContainer("margin-top", "1rem")
            .cssContainer("margin-bottom", "5rem")
            .print()
            .center()
            .wait(getSelectionGrid("test-phase").test.selectAny())
    )
)
