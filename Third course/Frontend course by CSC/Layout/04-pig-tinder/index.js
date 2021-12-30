let pigs;

let srcset;
let src;
let currentPig;

document.addEventListener('DOMContentLoaded', function() {
  src = 'image/<name>';
  currentPig = 0;

  onAction();
});

pigs = [
  {
    name: 'Маша',
    age_months: 6,
    image: 'masha.jpg'
  },
  {
    name: 'Катя',
    age_months: 7,
    image: 'katya.jpg'
  },
  {
    name: 'Ира',
    age_months: 8,
    image: 'ira.jpg'
  },
  {
    name: 'Лизка',
    age_months: 9,
    image: 'lizka.jpg'
  },
  {
    name: 'Вика',
    age_months: 10,
    image: 'vika.jpg'
  },
  {
    name: 'Антониха',
    age_months: 11,
    image: 'antoniha.jpg'
  },
  {
    name: 'Варя',
    age_months: 12,
    image: 'varya.jpg'
  }
];

function onAction() {
  const pig = pigs[currentPig];
  animate(false);
  document.getElementById('card-image').src = src.replaceAll('<name>', pig.image);
  document.getElementById('card-name').innerHTML = pig.name;
  document.getElementById('card-age').innerHTML = pig.age_months + ' месяцев';
  currentPig = (currentPig + 1) % pigs.length
  animate(true);
}

function animate(animIn) {
  const elem = document.getElementById('card');
  let op = animIn ? 0 : 1;
  let step = 0.05;
  step = animIn ? step : -step;
  const id = setInterval(() => {
    if ((animIn && op >= 1) || (!animIn && op <= 0)) {
      clearInterval(id);
    } else {
      op += step;
      elem.style.opacity = op;
    }
  }, 10);
}
