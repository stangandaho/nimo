var simple_lft = L.map('occ_data_access').setView([11, 1.8], 8);
L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: 'Map data © <a href="https://openstreetmap.org">OpenStreetMap</a> contributors'
}).addTo(simple_lft);

var miniMap = new L.Control.MiniMap(
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: 'Map data © <a href="https://openstreetmap.org">OpenStreetMap</a> contributors'
    }), {
        position: 'bottomleft',
        height: 100,
        width: 100
    }
).addTo(simple_lft);

var drawControl = new L.Control.Draw({
    draw: {
        marker: false,
        polyline: false,
        circleMarker: false,
        circle: false,
        rectangle: false
    },
    edit: {
        featureGroup: drawGroup
    }
}).addTo(simple_lft);

var measureControl = new L.Control.Measure({
    position: 'bottomleft',
    primaryLengthUnit: 'meters',
    primaryAreaUnit: 'sqmeters',
    activeColor: '#3D535D',
    completedColor: '#7D4479'
}).addTo(simple_lft);
