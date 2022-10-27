/**
* Stringmanager allows filename and projecttitle to be appended to the app's topbar
*/

function StringManager(settings){
    this.element = settings.element || null;
    this.separator = settings.separator || ' | ';
    this.projectString = settings.projectString;
    this.fileString = settings.fileString;
    this.nameString = settings.nameString;
    this.useDataClicked = false;
}

StringManager.prototype.setProject = function(string){
    this.projectString = string;
    this.updateElement();
};

StringManager.prototype.setFile = function(string){
    this.fileString = string;
    this.updateElement();
};

StringManager.prototype.setName = function(string){
    this.nameString = string;
    this.updateElement();
};


StringManager.prototype.updateElement = function(){
    var strings = [];

    if(this.projectString) strings.push(this.projectString);
    if(this.fileString) strings.push(this.fileString);
    if(this.nameString) strings.push(this.nameString);

    this.element.innerHTML = strings.join(this.separator);
};

var setProjectButton = document.getElementById('crtPrj'),
   //setNameButton = document.getElementById('setData'),
   //setFileButton = document.getElementById('setData'),
   projectContainer = document.createElement('span'),
   nav = document.querySelector('.navbar-static-top'),
   titles = new StringManager({
   		element: projectContainer,
   		separator: ' | ',
   		projectString: 'Project: <span style="color:#00783A">None</span>',
   		fileString: 'File: <span style="color:#00783A">None</span>',
   		nameString: ''
   	});

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
titles.updateElement();

window.addEventListener('load',function(){
	var prjNode = document.getElementById('prjName'),
		
	prjObserver = new MutationObserver(function(mutationsList) {
	    for(var mutation of mutationsList) {
	        if (mutation.type == 'childList') {
	            titles.setProject('Project: <span style="color:#fff">'+ prjNode.textContent + ' </span>');
	        }
	    }
	});

	prjObserver.observe(prjNode, { childList: true });
	
	nav.appendChild(projectContainer);
});


window.addEventListener("click",function(e){
  if(!e.target.closest('[id="setData"]')) return;
  
  titles.useDataClicked = true;
  var node = document.getElementById('fileName');
  titles.setFile('File: <span style="color:#fff">'+ node.textContent + ' </span>');
});


window.addEventListener('load',function(){
	var nameNode = document.getElementById('manualName'),
		
	nameObserver = new MutationObserver(function(mutationsList) {
	    for(var mutation of mutationsList) {
	        if (mutation.type == 'childList') {
	            titles.setName('Name: <span style="color:#fff">'+ nameNode.textContent + ' </span>');
	        }
	    }
	});

	nameObserver.observe(nameNode, { childList: true });
	
	nav.appendChild(projectContainer);
});

