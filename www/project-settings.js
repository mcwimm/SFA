/**
* Stringmanager allows filename and projecttitle to be appended to the app's topbar
*/

function StringManager(settings){
    this.element = settings.element || null;
    this.separator = settings.separator || ' | ';
    this.projectString = settings.projectString;
    this.fileString = settings.fileString;
}

StringManager.prototype.setProject = function(string){
    this.projectString = string;
    this.updateElement();
};

StringManager.prototype.setFile = function(string){
    this.fileString = string;
    this.updateElement();
};

StringManager.prototype.updateElement = function(){
    var strings = [];

    if(this.projectString) strings.push(this.projectString);
    if(this.fileString) strings.push(this.fileString);

    this.element.textContent = strings.join(this.separator);
};

var setProjectButton = document.getElementById('crtPrj'),
	setNameButton = document.getElementById('setData'),
	projectContainer = document.createElement('span'),
	nav = document.querySelector('.navbar-static-top'),
    	titles = new StringManager({
		element: projectContainer,
		separator: ' | ',
		fileString: 'File: Default',
		projectString: 'Project: No project chosen'
	});

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
projectContainer.innerHTML = 'Project: <span style="color:#00783A">No project chosen</span> | File: <span style="color:#00783A">Default</span>';


window.addEventListener('load',function(){
	var targetNode = document.getElementById('prjName'),
		config = { childList: true },
		observer = new MutationObserver(function(mutationsList, observer) {
		    for(var mutation of mutationsList) {
		        if (mutation.type == 'childList') {
		            titles.setProject('Project: '+ targetNode.textContent)
		        }
		    }
		});

	observer.observe(targetNode, config);

	setNameButton.addEventListener('click', function(){
		fileName = document.querySelector('[for="file1"] + .input-group > input').value;
      titles.setFile('File: '+ fileName)
   })
	
	nav.appendChild(projectContainer);

});


