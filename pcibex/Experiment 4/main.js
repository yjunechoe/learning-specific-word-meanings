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
    newSallyMessageBar("directions", "")
      .cssContainer("display", "block")
      .print()
      .css("display", "none")
    ,
    newSallyCanvas("training-message")
      .sallySay("Hi there!<br>My name is Sally.")
    ,
    newText("press-space", "<p class='PennController-blinking-text'>Press the spacebar key to proceed ...</p>")
      .center()
      .cssContainer("margin-top", "1em")
      .print()
    ,
    newKey("Next", " ")
      .wait()
    ,
    getSallyCanvas("training-message")
        .sallySay("We'll be play a game of finding matches.<br>Here's how it works:")
    ,
    getKey("Next")
      .wait()
    ,
    getSallyCanvas("training-message")
      .sallySay("I will first give you a word, and then<br>you will tell me if you find a match!")
    ,
    getKey("Next")
      .wait()
    ,
    getSallyCanvas("training-message")
      .sallySay("Are you ready for an example?")
    ,
    getKey("Next")
      .wait()
    ,
    getSallyCanvas("training-message")
      .css("visibility", "hidden")
    ,
    getSallyMessageBar("directions")
      .sallySay("You will now be seeing images appear below, one-by-one.")
      .css("display", "")
    ,    
    getKey("Next")
      .wait()
    ,
    getSallyMessageBar("directions")
      .sallySay('If what you see matches the word, press the "J" key.')
    ,
    getKey("Next")
      .wait()
    ,
    getSallyMessageBar("directions")
      .sallySay('If what you see does NOT match the word, press the "F" key.')
    ,
    getKey("Next")
      .wait()
    ,
    getSallyMessageBar("directions")
      .sallySay('In this example, the word is "pizza".')
    ,
    getKey("Next")
      .wait()
    ,
    getSallyMessageBar("directions")
      .sallySay('Keep your fingers on these keys and respond quickly!')
    ,
    getKey("Next")
      .wait()
    ,
    getSallyCanvas("training-message")
      .css("visibility", "hidden")
    ,
    getText("press-space")
      .remove()
    ,
    getSallyMessageBar("directions")
      .sallySay('Click the "Begin" button when you are ready.')
    ,
    newButton("Begin")
      .css({
        "position" : "absolute",
        "top" : "-350px",
        "font-size": "20px",
        "padding": "10px"
      })
      .cssContainer({
        "position": "relative"
      })
      .center()
      .print()
      .wait()
)

PennController("training",
    newSallyMessageBar("directions", "pizza")
      .sallyQuiet()
      .print()
    ,
    newTimer("Init", 500)
      .start()
      .wait()
    ,
    getSallyMessageBar("directions")
      .sallySay()
    ,
    newSelectionArray("training-phase", [0,1,2,3,4,5].map(i => "TRAINING/training-pizza" + i + ".jpg"))
      .print()
      .log()
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
        .sallySay("Now let's play this game with words<br>from my native language!")
    ,
    newText("press-space", "<p class='PennController-blinking-text'>Press the spacebar key to proceed ...</p>")
      .center()
      .cssContainer("margin-top", "1em")
      .print()
    ,
    newKey("Next", " ")
      .wait()
    ,
    getSallyCanvas("beginning-message")
        .sallySay("You've probably never seen these words before,<br>so I'll tell you what they mean first.")
    ,
    getKey("Next")
      .wait()
    ,
    getSallyCanvas("beginning-message")
        .sallySay('Remember to keep your fingers on "F" and "J" keys of your keyboard to make your choices!')
    ,
    getKey("Next")
      .wait()
    ,
    getText("press-space")
      .remove()
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
        newSallyMessageBar("directions", row[[row.target]])
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
        // keep up all taught referents
        getSallyCanvas("learn-phase")
            .showFirst()
            .sallyStill()
            .hideSpeechBubble()
        ,
        // -- Drill in target label
        newTimer("transition-1", 1000)
          .start()
          .wait()
        ,
        getSallyCanvas("learn-phase")
          .showSpeechBubble()
          .sallySay("Now can you help me find more " + row[[row.target]] + "s?")
        ,
        newTimer("transition-2", 5000)
          .start()
          .wait()
        ,
        // --
        // move to test phase after 3 sec
        getSallyCanvas("learn-phase")
          .remove()
        ,
        getSallyMessageBar("directions")
          .visible()
          .sallyQuiet()
        ,
        newTimer("show-selection-aray", 1000)
            .start()
            .wait()
        ,
        getSallyMessageBar("directions")
            .sallySay()
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
    .log("target", row.target)
    .log("order", row.order)
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
    ,
    newText( "debrief" , `<div>
        <h1>Debriefing</h1>
        <p>In this experiment, we were looking at how people generalize the meaning of a new word (e.g., "fep") when the evidence (e.g., an image of a dalmatian) points to either a narrow meaning (like DALMATIAN) or a broad meaning (like DOG) of the word. We hypothesized that when the evidence was also accompanied by an alternative to the narrow meaning (e.g., the word "blicket" presented with an image of a corgi), people are more likely to think that "fep" means DALMATIAN, as opposed to DOG.</p>
        <p>We used a word-picture matching task (the game you played with Sally) to test this hypothesis. If you answered "YES" just to images of other dalmatians, we took that to mean that you think "fep" means DALMATIAN. On the other hand, if you answered "YES" to all dogs, including dalmatians, we took that to mean that you think "fep" means DOG. Additionally, we also kept track of the time it took for you to make a decision with the F and J keys, to measure how confident or surprised you were in making a decision for each image during the task.</p>
    </div>`)
        .css({
          "width": "800px",
          "margin-top": "20px",
          "border-top": "2px solid black"
        })
        .center()
        .print()
    ,
    newButton("empty")
        .print()
        .hidden()
        .wait()
).setOption("hideProgressBar",true)
