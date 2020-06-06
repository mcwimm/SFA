var setProjectButton = document.getElementById('crtPrj'),
	projectContainer = document.createElement('span'),
	nav = document.querySelector('.navbar-static-top');

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
projectContainer.innerHTML = 'Project: <span style="color:#f00">No project chosen</span>';

nav.appendChild(projectContainer);


function showProjectInHeader(prjName) {
	projectContainer.textContent = 'Project: ' + prjName;
}


window.addEventListener('load',function(){
	var targetNode = document.getElementById('prjName'),
		config = { childList: true },
		observer = new MutationObserver(function(mutationsList, observer) {
		    for(var mutation of mutationsList) {
		        if (mutation.type == 'childList') {
		            showProjectInHeader(targetNode.textContent)
		        }
		    }
		});

	observer.observe(targetNode, config);
});