PennController.ResetPrefix(null) 
DebugOff()
var showProgressBar = false;

PennController.Sequence("intro", "counter", "instructions", "training", "beginning", "experiment", "end", SendResults(), "bye")

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

PennController("instructions",
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
    newSallyCanvas("training-message")
        .sallySay("Hi there!<br>My name is Sally.")
    ,
    newButton("Next-1", "Next")
    ,
    getSallyCanvas("training-message")
        .sallySay("We'll be play a game of finding matches.<br>Here's how it works:")
    ,
    newButton("Next-2", "Next")
    ,
    getSallyCanvas("training-message")
        .sallySay("I will first give you a word, and then<br>you will tell me if you find a match!")
    ,
    newButton("Next-3", "Next")
    ,
    getSallyCanvas("training-message")
      .sallySay("Are you ready for an example?")
    ,
    newButton("Next-4", "Next")
    ,
    getSallyCanvas("training-message")
      .css("display", "none")
    ,
    newSallyMessageBar("directions", "")
      .cssContainer("display", "block")
      .sallySay("You will now be seeing images appear below, one-by-one.")
      .print()
    ,    
    newButton("Next-5", "Next")
    ,
    getSallyMessageBar("directions")
      .sallySay('If what you see matches the word, press the "F" key.')
    ,
    newButton("Next-6", "Next")
    ,
    getSallyMessageBar("directions")
      .sallySay('If what you see does NOT match the word, press the "J" key.')
    ,
    newButton("Next-7", "Next")
    ,
    getSallyMessageBar("directions")
      .sallySay('In this example, the word is "pizza".')
    ,
    newButton("Next-8", "Next")
    ,
    getSallyMessageBar("directions")
      .sallySay('Keep your fingers on these keys and respond quickly!')
    ,
    newButton("Next-9", "Next")
    ,
    getSallyMessageBar("directions")
      .sallySay('Click the "Begin" button when you are ready.')
    ,
    newButton("Begin", "Begin")
)

PennController("training",
    newTimer("timer", 500)
      .start()
      .wait()
    ,
    newSallyMessageBar("directions", "pizza")
      .print()
    ,
    newSelectionArray("training-phase", [0,1,2,3,4,5].map(i => "TRAINING/training-pizza" + i + ".jpg"))
      .print()
      .center()
      .show()
    ,
    newButton("Continue")
      .center()
      .print()
      .cssContainer({"position": "relative"})
      .css({
        "position" : "absolute",
        "top" : "-200px",
        "transform": "translateX(-50%)",
        "z-index": "2",
        "font-size": "24px",
        "padding": "10px"
      })
      .wait()
      .remove()
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
        .sallySay("Now let's play this game with words from my native language.")
    ,
    newButton("Next-1", "Next")
    ,
    getSallyCanvas("beginning-message")
        .sallySay("You've probably never seen these words before,<br>so I'll teach you what they mean first.")
    ,
    newButton("Next-2", "Next")
    ,
    getSallyCanvas("beginning-message")
        .sallySay('Remember to keep your fingers on "F" and "J" keys of your keyboard to make your choices!')
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
        newSallyMessageBar("directions", row.label1)
            .cssContainer("display", "block")
            .print()
            .hidden()
        ,
        newButton("Continue")
            .center()
            .cssContainer({"position": "relative"})
            .css({
                "position" : "absolute",
                "top" : "-200px",
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
        // -- Drill in target label
        newTimer("transition-1", 1000)
          .start()
          .wait()
        ,
        getSallyCanvas("learn-phase")
          .showFirst()
          .sallyStill()
          .showSpeechBubble()
          .sallySay("Now can you help me find more " + row.label1 + "s?")
        ,
        newTimer("transition-2", 5000)
          .start()
          .wait()
        ,
        // --
        // move to test phase after 3 sec
        newTimer("show-selection-aray", 1000)
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
    .log("contrast", row.contrast)
    .log("other_set", row.other_set)
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
