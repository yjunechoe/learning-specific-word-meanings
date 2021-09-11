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
        newSelectionGrid("test-phase", row.choices)
            .print()
            .hide()
            .log()
        ,
        newTimer("hurry", 3000)
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
