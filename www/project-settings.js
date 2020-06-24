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
}
StringManager.prototype.setFile = function(string){
    this.fileString = string;
    this.updateElement();
}
StringManager.prototype.updateElement = function(){
    var strings = [];

    if(this.projectString) strings.push(this.projectString);
    if(this.fileString) strings.push(this.fileString);

    this.element.textContent = strings.join(this.separator);
}

var setProjectButton = document.getElementById('crtPrj'),
	setNameButton = document.getElementById('setData'),
	projectContainer = document.createElement('span'),
	nav = document.querySelector('.navbar-static-top'),
    	titles = new Stringmanager({
		element: projectContainer,
		separator: ' | ',
	}),;

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
<<<<<<< HEAD
projectContainer.innerHTML = 'Project: <span style="color:#f00">No project chosen </span>';

nav.appendChild(projectContainer);


function showProjectInHeader(prjName) {
	projectContainer.textContent = 'Project: ' + prjName;
}

function showFileNameInHeader(fileName) {
	projectContainer.textContent = projectContainer.textContent + ' | File : ' + fileName;
}


window.addEventListener('load', function(){
=======
projectContainer.innerHTML = 'Project: <span style="color:#f00">No project chosen</span> | File: <span style="color:#f00">Default</span>';

nav.appendChild(projectContainer);

window.addEventListener('load',function(){
>>>>>>> 9982fb4c1ed66c85556879bf6aff6bf4ad834e19
	var targetNode = document.getElementById('prjName'),
		config = { childList: true },
		observer = new MutationObserver(function(mutationsList, observer) {
		    for(var mutation of mutationsList) {
		        if (mutation.type == 'childList') {
		            titles.setProject('Project: '+targetNode.textContent)
		        }
		    }
		});

	observer.observe(targetNode, config);
<<<<<<< HEAD

	setNameButton.addEventListener('click', function(){
		fileName = document.querySelector('[for="file1"] + .input-group > input').value;
		showFileNameInHeader(fileName);
	})
});


=======
});
>>>>>>> 9982fb4c1ed66c85556879bf6aff6bf4ad834e19
