// --------------- Map of disease frequency ---------------
$(function () {
	$("#map2").attr('tabindex',-1);
	$("svg").attr('role', 'img');
    $("svg").attr('aria-label', 'map application');
});
var col = ["#ff7272","#2a637e","#bda06a" ,  "#72c2ff",  "#ca6293", "#40c2c4", "#fd4d77", "#307981",  "#ff9147"];

function download() {
	var element = document.createElement('a');
	element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
	element.setAttribute('download', 'studylist.txt');
	element.style.display = 'none';
	document.body.appendChild(element);
	element.click();
	document.body.removeChild(element);
}
var text = '';
function getCurrentFeature(e) {
	var layer = e.target;
	text = layer.feature.properties.downlaod_content;
}

function onEachPolygon_map2 (feature, layer, c){
	var dt = data2.filter(function (d){
		return d.OfficialName === feature.properties.name0 && d.studyContext===c;
	});

	var popupContent = '<button type="button" id="btnDown" onclick="download()">Download list</button>' ;
	var download_content='';

	for(k= 0; k< dt.length; k++){
		popupContent += '<br><table>';
		popupContent += '<tr><td>Disease</td><td>'+ setup.disease +'</td></tr>';
		popupContent += '<tr><td>Country</td><td>'+ dt[k].OfficialName +'</td></tr>';
		popupContent += '<tr><td>Sample Area</td><td>'+ dt[k].sampArea +'</td></tr>';
		popupContent += '<tr><td>Target Species</td><td>'+ dt[k].targetSpecies +'</td></tr>';
		popupContent += '<tr><td>Publication Year</td><td>'+ dt[k].publicationYear +'</td></tr>';
		if(dt[k].DOI != '')
			popupContent += '<tr><td>Bibliography</td><td><a href="'+ dt[k].DOI + '" target="_blank">' + dt[k].Bibliography +'</a></td></tr>';
			else
				popupContent += '<tr><td>Bibliography</td><td>'+ dt[k].Bibliography +'</td></tr>';

				popupContent +='</table>';

				download_content += 'Study: ' + (k+1);
				download_content += '\nDisease: '+ setup.disease;
				download_content += '\nCountry: '+ dt[k].OfficialName ;
				download_content += '\nSample Area: '+ dt[k].sampArea;
				download_content += '\nTarget Species: '+ dt[k].targetSpecies;
				download_content += '\nN.Positive: '+ dt[k].nPositive;
				download_content += '\nPublication Year: '+ dt[k].publicationYear;
				download_content += '\nBibliography: '+ dt[k].Bibliography + '\n\n';
	}
	feature.properties.downlaod_content= download_content;

	var popup = layer.bindPopup(popupContent);
	popup.on("popupclose", function(e) {
		resetHighlight(e);
	});
}

var streets2 = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}', {
	attribution: 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ &copy; <a href="https://data.europa.eu/">Joint Research Centre</a>',
	maxZoom: 16
});

var map2 = L.map('map2', {
	center: [45, 40],
	zoom: 2,
	layers: [streets2],
	minZoom: 2,
	maxBounds: L.latLngBounds(L.latLng(-90, -180), L.latLng(90, 180))
});
var baseLayer2 = {};
var overlays_map2= {};
const context = [...new Set(data2.map(item => item.studyContext))];
for ( i in context) {
	this['layer_map2_'+i] = new L.featureGroup;
	overlays_map2[ '<span class="lyr-con-label" style="background:' + col[i] + '99">'+context[i] +'</span>' ] = window['layer_map2_'+i];
}

for(i in context){
	var dt2 = data2.filter(function (d){
		return d.studyContext === context[i]
	});
	L.geoJson(country2, {
		filter: function (feature, layer) {
			var dt2_cntr = [...new Set(dt2.map(item => item.OfficialName))];
			return ( dt2_cntr.includes(feature.properties.name0) )
		},
		style:function (feature) {
			return {
				fillColor: col[i],
				weight: 1.5,
				opacity: 0.2,
				color: 'gray',
				fillOpacity: 0.5
			};
		},
		onEachFeature: function(feature, layer) {
			onEachPolygon_map2(feature, layer, context[i]);
			layer.on({
				click: function(e) { highlightFeature(e); getCurrentFeature(e) }
			})
		}
	}).addTo(window['layer_map2_'+i]);
}
for(i in overlays_map2){
	overlays_map2[i].addTo(map2);
}


function rmMap2Control() {
	$("#map2 .leaflet-control-layers").hide();
	swMap2Control.addTo(map2);
}

var swMap2Control = L.easyButton('fa fa-edit fa-2x',
								 function (btn, map2) {
									 $("#map2 .leaflet-control-layers").show();
									 map2.removeControl(swMap2Control);
								 },
								 { position: 'topright'}
);
var map2LyrControl = L.control.layers(baseLayer2, overlays_map2, {collapsed:false, position: 'topright'}).addTo(map2);
$("#map2 .leaflet-control-layers").children().prepend('<div><a class = "control-close-button" href="javascript:rmMap2Control()">&#xd7; </a><br><h6 class="lyrName">Study context</h6></div>');
map2.zoomControl.setPosition('topleft');
