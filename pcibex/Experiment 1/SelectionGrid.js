window.PennController._AddElementType("SelectionGrid", function(PennEngine) {
    
    var selection = new Set()

    this.immediate = function(id, imageString){
        
        // debug ---
        // imageString = '["vehicle/sup-vehicle-5.jpg","beast/basic-dinosaur/basic-dinosaur-1.jpg","light/lamp/contrast-desk_lamp/contrast-desk_lamp-2.jpg","fruit/basic-apple/sub-red_apple/sub-red_apple-1.jpg","light/lamp/basic-lamp-2.jpg","light/lamp/sub-gooseneck_lamp/1sub-gooseneck_lamp.jpg","fruit/basic-apple/basic-apple-2.jpg","electronics/sup-electronics-5.jpg","furniture/basic-chair/basic-chair-2.jpg","beast/basic-dinosaur/contrast-triceratops/contrast-triceratops-1.jpg","vehicle/basic-car/sub-racecar/1sub-racecar.jpg","beast/sup-beast-1.jpg","furniture/sup-furniture-4.jpg","fruit/basic-apple/sub-red_apple/1sub-red_apple.jpg","electronics/basic-phone/sub-smart_phone/sub-smart_phone-1.jpg","vehicle/sup-vehicle-4.jpg","furniture/basic-chair/contrast-office_chair/contrast-office_chair-1.jpg","electronics/sup-electronics-4.jpg","beast/basic-dinosaur/sub-trex/sub-trex-2.jpg","vegetable/basic-pepper/contrast-red_pepper/1contrast-red_pepper.jpg"]'
        console.log(imageString)
        
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
                            console.log(selection)
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
            console.log("SelectionGrid hidden")
            this.jQueryElement.css("display", "none")
            resolve();
        },
        show: function(resolve){
            console.log("SelectionGrid shown")
            this.jQueryElement.css("display", "grid")
            resolve();
        }
    }


})