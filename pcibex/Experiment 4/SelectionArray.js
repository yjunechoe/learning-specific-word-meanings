window.PennController._AddElementType("SelectionArray", function(PennEngine) {
    
    this.immediate = function(id, imageString){
        
        this.id = id;
        
        // image properties
        this.imgPathArray = typeof imageString == "string" ? JSON.parse(imageString) : imageString;
        
        // console.log(this.imgPathArray)
        
        // preload images
        this.imgArray = this.imgPathArray.slice().map(d => {
            let imgObj = new Image();
            imgObj.src = "https://raw.githubusercontent.com/yjunechoe/learning-specific-word-meanings/main/image_stimuli/" + d;
            return imgObj;
        })
        
        // selections
        this.selections = [];
        // [img, click type, time]
        this.times = [];
    }
        
    // render
    this.uponCreation = function(resolve){
        
        this.completed = false;

        that = this;
        // define grid
        // append images
        //
        this.jQueryElement = $("<div>")
          .attr("class", "PennController-SelectionArray")
          .append($(`
              <div class="PennController-SA-time-bar-wrapper">
                <div class="PennController-SA-time-bar">
                </div>
              </div>
              <div class="PennController-SA-positioner">
                <div class="PennController-SA-wrapper">
                </div>
              </div>
              <div class="PennController-SA-actions">
                <button class="PennController-SA-btn">Yes</button>
                <button class="PennController-SA-btn">No</button>
              </div>
          `))
          
        const wrapper = this.jQueryElement.find(".PennController-SA-wrapper")
        const cards = this.jQueryElement.find(".PennController-SA-card")
        
        this.imgArray.forEach(img => {
            const card = $(`<div class="PennController-SA-card"><img class="PennController-SA-card-img" src=${img.src}></div>`)
            wrapper.append(card)
        })
        
        const cardImg = this.jQueryElement.find(".PennController-SA-card-img")
        const timeBar = this.jQueryElement.find(".PennController-SA-time-bar-wrapper div")
        const buttons = this.jQueryElement.find(".PennController-SA-actions button")

        const move_on = function() {
          const len = wrapper.children().length
          const cur = wrapper.children()[len - 1]
          console.log(len)
          console.log(that.selections)
          console.log(that.times)
          cur.remove()
          clearInterval(that.barTime)
          if (len > 1) {
            that.barTime = that.barShrink()
          } else {
            buttons.css("display", "none")
            timeBar.css("visibility", "hidden")
          }
        }
        
        // Timer logic
        this.barShrink = function() {
          timeBar.css({"width": "500px"})
          var time = 0;
          var tick = setInterval(increment, 10)
          function increment() {
            if (time > 300) {
              that.selections.push("NONE")
              that.times.push(time)
              clearInterval(tick)
              move_on()
            } else {
              time++;
              timeBar.css({"width": 500 - 500/300 * time + "px"})
            }
          }
          return {stop: function() {
            clearInterval(tick)
            return time
          }}
        }
        
        // Button logic
        buttons.click(event => {
            this.selections.push(event.target.textContent)
            this.times.push(this.barTime.stop())
            move_on()
        })
        
        resolve();

    };
    
    this.end = function(){
        // log
        if (this.log){
            let trialResult = Array.from(that.selections).join(";") + "|" + Array.from(that.times).join(";")
            console.log(trialResult)
            PennEngine.controllers.running.save(this.type, this.id, "Selections", trialResult, this.printTime, "NULL")
        }
        // remove all elements from selection set
        that.selections = []
        
    };

    this.test = {
    };
    
    // display none so imgs load during test but doesn't takeup space
    this.actions = {
        hide: function(resolve){
            // console.log("SelectionGrid hidden")
            this.jQueryElement.css("display", "none")
            resolve();
        },
        show: function(resolve){
            // console.log("SelectionGrid shown")
            this.jQueryElement.css("display", "grid")
            this.startTime = Date.now()
            this.barTime = this.barShrink()
            resolve();
        }
    }
    
})