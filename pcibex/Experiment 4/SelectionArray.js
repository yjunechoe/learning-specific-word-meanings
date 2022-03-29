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
        // reverse because cards are FIFO
        this.imgArray.reverse()
        
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
                <button class="PennController-SA-btn PennController-SA-btn-yes">
                  <span>Yes<br>"F"</br></span>
                </button>
                <button class="PennController-SA-btn PennController-SA-btn-no">
                  <span>No<br>"J"</br></span>
                </button>
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
        const buttons = this.jQueryElement.find("button.PennController-SA-btn")
        const buttonYes = this.jQueryElement.find("button.PennController-SA-btn-yes")
        const buttonNo = this.jQueryElement.find("button.PennController-SA-btn-no")

        // Card traversing logic
        const move_on = () => {
          const len = wrapper.children().length
          const cur = wrapper.children()[len - 1]
          console.log(len)
          console.table({response: that.selections, time: that.times})
          cur.remove()
          clearInterval(that.barTime)
          if (len > 1) {
            that.barTime = that.barShrink()
          } else {
            document.removeEventListener("keydown", this.keydown_fj)
            buttons.css("display", "none")
            timeBar.css("visibility", "hidden")
            // replace sally message bar text
            document.querySelector("p.PennController-speech-text").textContent = 'Good work! Click the "Continue" button to proceed.'            }
        }
        
        // Keydown logic (defined as property)
        this.keydown_fj = e => {
          e.preventDefault
          const key = e.key.toLowerCase()
          if (key == "f") {
              this.selections.push("Yes")
              this.times.push(this.barTime.stop())
              buttonYes.removeClass("PennController-SA-btn-clicked")
              setTimeout(() => {
                buttonYes.addClass("PennController-SA-btn-clicked")
              }, 10)
              move_on()
          } else if (key == "j") {
              this.selections.push("No")
              this.times.push(this.barTime.stop())
              buttonNo.removeClass("PennController-SA-btn-clicked")
              setTimeout(() => {
                buttonNo.addClass("PennController-SA-btn-clicked")
              }, 10)
              move_on()
          }
        }
        
        // Timer logic
        this.barShrink = function() {
          timeBar.css({"width": "500px"})
          var time = 0;
          var tick = setInterval(increment, 10)
          function increment() {
            if (time >= 5000) {
              that.selections.push("NONE")
              that.times.push(time)
              clearInterval(tick)
              move_on()
            } else {
              time = time + 10;
              timeBar.css({"width": 500 - (time/10) + "px"})
            }
          }
          return {stop: function() {
            clearInterval(tick)
            return time
          }}
        }
        
        // Button logic
        // buttons.click(event => {
        //    this.selections.push(event.target.textContent)
        //    this.times.push(this.barTime.stop())
        //    move_on()
        // })
        
        // Keypress logic
        
        resolve();

    };
    
    this.end = function(){
        // log
        if (this.log){
            let trialResult = Array.from(that.selections).join(";") + "|" + Array.from(that.times).join(";")
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
            document.addEventListener("keydown", this.keydown_fj)
            this.startTime = Date.now()
            this.barTime = this.barShrink()
            resolve();
        }
    }
    
})
