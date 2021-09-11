window.PennController._AddElementType("SallyCanvas", function(PennEngine) {
    
    this.immediate = function(id, learningString = '["animal/basic-dog/sub-dalmatian/1sub-dalmatian.jpg", "animal/basic-dog/contrast-corgi/1contrast-corgi.jpg"]'){
        
        this.id = id;
        
        // image properties
        this.learningString = typeof learningString == "string" ? JSON.parse(learningString) : learningString;
        
        // sally imgs
        this.sallyArray = ["Sally/pointer-none.png", "Sally/pointer-left.png", "Sally/pointer-right.png"].map(d => {
            return "https://raw.githubusercontent.com/yjunechoe/learning-specific-word-meanings/main/image_stimuli/" + d
        })
        // preload images
        this.sallyImgArray = this.sallyArray.map(d => {
            let imgObj = new Image();
            imgObj.src = d;
            return imgObj;
        })
        
        this.learnFirstImg = this.learningString[0] || new Image();
        this.learnSecondImg = this.learningString[1] || new Image();
        
        this.sallyImg = $("<img>").attr("class", "PennController-sally-img").attr("src", this.sallyArray[0])        
        this.learnFirst = $("<div>").attr("class", "PennController-labelled PennController-left-ref")
        this.learnSecond = $("<div>").attr("class", "PennController-labelled PennController-right-ref")
        
        this.speechText = $("<div>").attr("class", "PennController-speech-text")
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
    };
    
    this.actions = {
        showSpeechBubble: function(resolve){
            this.speechBubble.css("display", "inherit")
            resolve();
        }
        // show sally none
        // show left img and sally-point-left
        // show right img and sally-point-right
        // hide show selectiongrid directions and sally-point-none
    };


})