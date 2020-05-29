var setProjectButton = document.getElementById('crtPrj'),
	projectContainer = document.createElement('span'),
	nav = document.querySelector('.navbar-static-top');

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
projectContainer.textContent = 'Project: -';

nav.appendChild(projectContainer);

function showProjectInHeader() {
	var prjName = document.getElementById('prjName').textContent;

	projectContainer.textContent = 'Project: ' + prjName;
}

window.addEventListener('load', function(){ 
	setProjectButton.addEventListener('click', showProjectInHeader);
});