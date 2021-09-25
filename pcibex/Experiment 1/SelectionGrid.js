window.PennController._AddElementType("SelectionGrid", function(PennEngine) {
    
    var selection = new Set()

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
        
        // console.log(this.imgArray)
        
        this.clickEventImg = new Array();
        this.clickEventTime = new Array();
        this.clickEventType = new Array();
    }
        
    // render
    this.uponCreation = function(resolve){

        // define grid
        // append images
        //
        this.jQueryElement = $("<div>")
          .attr("class", "PennController-SelectionGrid");
        
        this.imgArray.forEach(img => {
            let imgfile = img.src.match("[a-z0-9\-_]+\.jpg")[0]
            this.jQueryElement
                .append(
                    $("<div class='PennController-cell'>")
                        .append(
                            $(img).attr("img-file", imgfile)
                        )
                        .click(function(){
                            let cell = $(this)
                            if (cell.hasClass("PennController-cell-selected")) {
                                cell.removeClass("PennController-cell-selected")
                                selection.delete(imgfile)
                            } else {
                                cell.addClass("PennController-cell-selected")
                                selection.add(imgfile)
                            }
                            // console.log(selection)
                        })
                )
        })
        
        resolve();

    };
    
    this.end = function(){
        // log
        if (this.log){
            PennEngine.controllers.running.save(this.type, this.id, "Selections", Array.from(selection).join(";"), this.printTime, "NULL")
        }
        // remove all elements from selection set
        selection.clear()
        
    };

    this.test = {
        // test for any
        selectAny: function() {
			if (selection.size === 0) {
			    alert("Please make a selection.")
			}
            return selection.size > 0
        }
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
            resolve();
        }
    }


})