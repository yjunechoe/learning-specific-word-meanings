PennController.ResetPrefix(null)

PennController.Sequence("consent", "counter", "experiment")

SetCounter("counter", "inc", 1);


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
            .hideAll()
            .sallySay("Do you see any other " + row.label1 + "s below?<br>" + "Click on all the " + row.label1 + "s you see!")
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
