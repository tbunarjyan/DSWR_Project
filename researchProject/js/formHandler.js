


var distance=['Center', 'Arabkir', 'Shengavit', 'Avan', 'Malatia-Sebastia', 'Qanaqer-Zeytun',
    'Nor Norq', 'Achapnyak', 'Davtashen', 'Erebuni', 'Norq Marash', 'Nubarashen',
    'Vahagni district'];
var weigts=[-1.40715056e+01, 1.45043185e+00, -1.43152008e+01, 7.15961455e+01,
    1.98853809e+01,  -7.28438671e+00,   8.33862711e+00,  -1.51257458e+01,
    0.00000000e+00,  -1.40715056e+01,  -1.40715056e+01,  -1.40715056e+01,
    -1.40715056e+01,  -1.40715056e+01,  -1.40715056e+01,  -1.40715056e+01,
    -1.40715056e+01,  -1.40715056e+01,  -1.40715056e+01,  -1.40715056e+01,
    -1.40715056e+01,   0.00000000e+00,  -1.40715056e+01,  -1.40715056e+01,
    -1.40715056e+01,  -1.27997209e-02];
var building=['panel', 'monolit', 'other' ,'stone'];




function myfunction() {
    var features=[];
    features[0]=1;
    features[1]=parseFloat(document.getElementById('ar').value);
    features[2]=parseFloat(document.getElementById('nr').value);
    features[3]=parseFloat(document.getElementById('ch').value);
    features[4]=parseFloat(document.getElementById('nb').value);
    features[5]=features[6]=features[7]=0;

    if(document.getElementById("options1").checked = true)
        features[5] = 1;
    else{
        if(document.getElementById("options2").checked = true)
            features[6]=1;
        else
            features[7]=1
    }
    for(var i=0;i<distance.length;i++){
        if(document.getElementById('d').value==distance[i])
            features[i+8]=1;
        else
            features[i+8]=0
    }

    for(var j=0;j<building.length;j++) {
        if (document.getElementById('bt').value== building[i])
            features[j + 21] = 1;
        else
            features[j + 21] = 0
    }
    var price=0;
    for(var k=0;k<features.length;k++){
        price+=features[k]*weigts[k];
    }

    document.getElementById("alert").innerHTML='The Suggested Price is &asymp; '+'$'+(Math.round(price))+' 000';
  //  alert)
    


}




