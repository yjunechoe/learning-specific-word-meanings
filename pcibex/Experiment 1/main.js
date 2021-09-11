PennController.ResetPrefix()

newTrial(
    newSelectionGrid("test-phase")
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