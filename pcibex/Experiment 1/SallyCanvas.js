window.PennController._AddElementType("SallyCanvas", function(PennEngine) {
    
    this.immediate = function(id, learningString = '["animal/basic-dog/sub-dalmatian/1sub-dalmatian.jpg", "animal/basic-dog/contrast-corgi/1contrast-corgi.jpg"]'){
        
        this.id = id;
        
        // image properties
        this.learningString = typeof learningString == "string" ? JSON.parse(learningString) : learningString;
        
        // sally imgs
        let remote = "https://raw.githubusercontent.com/yjunechoe/learning-specific-word-meanings/main/image_stimuli/"
        this.sallyArray = ["Sally/pointer-none.png", "Sally/pointer-left.png", "Sally/pointer-right.png"].map(d => {
            return remote + d
        })
        // preload images
        this.sallyImgArray = this.sallyArray.map(d => {
            let imgObj = new Image();
            imgObj.src = d;
            return imgObj;
        })
        
        this.learnFirstImg = $("<img>").attr("src", remote + this.learningString[0]);
        this.learnSecondImg = $("<img>").attr("src", remote + this.learningString[1]);
        
        this.sallyImg = $("<img>").attr("class", "PennController-sally-img").attr("src", this.sallyArray[0])        
        let learnFirst = $("<div>").attr("class", "PennController-labelled PennController-left-ref")
            .append(this.learnFirstImg)
        let learnSecond = $("<div>").attr("class", "PennController-labelled PennController-right-ref")
            .append(this.learnSecondImg)
        
        this.secondMissing = this.learningString.length === 1
        if (this.secondMissing) {
            learnSecond = null
        }
        
        this.learnFirst = learnFirst
        this.learnSecond = learnSecond

        this.speechText = $("<p>").attr("class", "PennController-speech-text")
        this.speechBubble = $("<div>").attr("class", "PennController-speech-bubble")
            .css("display", "none")
            .append(this.speechText)
    }
        
    // render
    this.uponCreation = function(resolve){

        // define canvas
        this.jQueryElement = $("<div>")
            .attr("class", "PennController-SallyCanvas")
            .append(this.sallyImg)
            .append(this.speechBubble)
            .append(this.learnFirst)
            .append(this.learnSecond)
        
        resolve();

    };
    
    this.end = function(){
    };

    this.test = {
        noContrast: function(){
            return this.secondMissing
        },
    };
    
    this.actions = {
        sallyStill: function(resolve){
            this.sallyImg.attr("src", this.sallyArray[0])
            resolve();
        },
        sallySay: function(resolve, speech){
            this.speechText.html(speech);
            resolve();
        },
        showSpeechBubble: function(resolve){
            this.speechBubble.css("display", "inherit");
            resolve();
        },
        hideSpeechBubble: function(resolve){
            this.speechBubble.css("display", "none");
            resolve(); 
        },
        showFirst: function(resolve){
            this.learnFirstImg.css("visibility", "visible");
            this.sallyImg.attr("src", this.sallyArray[1]) 
            resolve();
        },
        showSecond: function(resolve){
            this.learnSecondImg.css("visibility", "visible");
            this.sallyImg.attr("src", this.sallyArray[2])
            resolve();
        },
        hideAll: function(resolve){
            this.learnFirstImg.css("visibility", "hidden");
            this.learnSecondImg.css("visibility", "hidden");
            resolve();
        }
        // show sally none
        // show left img and sally-point-left
        // show right img and sally-point-right
        // hide show selectiongrid directions and sally-point-none
    };


})