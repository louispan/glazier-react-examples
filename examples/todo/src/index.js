// import React from 'react';
// import ReactDOM from 'react-dom';
import './index.css';
import '../node_modules/todomvc-common/base.css';
import '../node_modules/todomvc-app-css/index.css';
// import App from './App';
// import * as serviceWorker from './serviceWorker';

import hsMain from '../.shake/hsMain.js';

// This has the side effect of running the haskell main
hsMain();

// ReactDOM.render(<App />, document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();
