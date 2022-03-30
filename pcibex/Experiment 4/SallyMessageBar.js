window.PennController._AddElementType("SallyMessageBar", function(PennEngine) {
    
    this.immediate = function(id, target){
        
        this.id = id;
        this.target = target
        
    }
        
    // render
    this.uponCreation = function(resolve){

        // define canvas
        this.jQueryElement = $(`<div class='PennController-SallyCanvas PennController-learn-phase' style='height: 100px; width: 1100px; outline:none'> <img class='PennController-sally-img' src='https://raw.githubusercontent.com/yjunechoe/learning-specific-word-meanings/main/image_stimuli/Sally/pointer-none.png' style='height:100%; left:10%; right:0%; top:0%; position:relative'> <div class='PennController-speech-bubble small-speech-bubble' style='transform:translateY(-50%); top:50%; left:25%; display:block; border-radius:0.5rem'> <p class='PennController-speech-text'>Is this a ${this.target}?</p> </div></div><style>.PennController-speech-bubble.small-speech-bubble:before { content: ''; position: absolute; bottom: 50%; left: 0%; width: 20px; height: 20px; background-color: inherit; transform: translate(-54%, 50%) rotate(45deg); transform-origin: center center; border-width: 2px; border-style: solid; border-color: transparent transparent rgb(110, 166, 213) rgb(110, 166, 213); border-image: initial;}</style>`)
        
        this.textp = this.jQueryElement.find("p.PennController-speech-text")
        
        resolve();

    };
    
    this.end = function(){
    };

    this.test = {
    };
    
    this.actions = {
      sallySay: function(resolve, speech) {
        this.textp.text(speech)
        resolve()
      }
    };


})