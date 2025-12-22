const NUM_ROWS = 6;
const NUM_COLS = 7;

let currentBoard = [];
let currentPlayer = 'r';
let gameActive = true;
let gameMode = null;
let moveInProgress = false;
let iaTypeRed = 'human';
let iaTypeYellow = 'aiRand';

function showMenu() {
    document.getElementById('game-mode-selector').classList.remove('hidden');
    document.getElementById('ia-selector').classList.add('hidden');
    document.getElementById('game-container').classList.add('hidden');
}

function showHumanVsIA() {
    gameMode = 'human-vs-ia';
    iaTypeRed = 'human';
    document.getElementById('game-mode-selector').classList.add('hidden');
    document.getElementById('ia-selector').classList.remove('hidden');
    document.getElementById('human-vs-ia-section').classList.remove('hidden');
    document.getElementById('ia-vs-ia-section').classList.add('hidden');
}

function showHumanVsHuman() {
    gameMode = 'human-vs-human';
    iaTypeRed = 'human';
    iaTypeYellow = 'human';
    document.getElementById('game-mode-selector').classList.add('hidden');
    document.getElementById('ia-selector').classList.remove('hidden');
    document.getElementById('human-vs-ia-section').classList.remove('hidden');
    document.getElementById('ia-vs-ia-section').classList.add('hidden');
}

function showIAVsIA() {
    gameMode = 'ia-vs-ia';
    document.getElementById('game-mode-selector').classList.add('hidden');
    document.getElementById('ia-selector').classList.remove('hidden');
    document.getElementById('human-vs-ia-section').classList.add('hidden');
    document.getElementById('ia-vs-ia-section').classList.remove('hidden');
}

function showGame() {
    document.getElementById('ia-selector').classList.add('hidden');
    document.getElementById('game-container').classList.remove('hidden');
}

async function callAPI(endpoint, data = {}) {
    const res = await fetch(`/api/${endpoint}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data)
    });
    
    console.log(res);

    if (!res.ok) {
        const errorText = await res.text();
        console.error("API error:", res.status, errorText);
        throw new Error(`API ${endpoint} failed (${res.status})`);
    }

    return await res.json();
}

async function initGame() {
    const data = await callAPI('init_game', { red: iaTypeRed, yellow: iaTypeYellow });
    console.log('Board from API:', data.board);
    console.table(data.board);

    currentBoard = data.board;
    currentPlayer = data.currentPlayer;
    gameActive = true;

    createBoardUI();
    updateBoardUI();
    updatePlayerDisplay();

    if ((currentPlayer === 'y' && iaTypeYellow !== 'human') ||
        (currentPlayer === 'r' && iaTypeRed !== 'human')) {
        setTimeout(playIAMove, 800);
    }
}

async function playHumanMove(col) {
    if (!gameActive) return;
    if (moveInProgress) return;
    if ((currentPlayer === 'r' && iaTypeRed !== 'human') ||
        (currentPlayer === 'y' && iaTypeYellow !== 'human')) return;

    moveInProgress = true;
    updatePlayerDisplay();

    try {
        const data = await callAPI('play_move', { column: col });
        handleMove(data);
    } catch (err) {
        console.warn("Human move refused:", err.message);
    } finally {
        moveInProgress = false;
        updatePlayerDisplay();
    }
}

async function playIAMove() {
    if (!gameActive) return;

    try {
        const data = await callAPI('ia_move');
        handleMove(data);
    }   catch (err) {
        console.warn("IA move refused:", err.message);
    }
    
}

function handleMove(data) {
    if (!data || !data.board) {
        console.error('Invalid response from server', data);
        document.getElementById('game-status').textContent = 'Erreur serveur';
        gameActive = false;
        return;
    }

    currentBoard = data.board;
    currentPlayer = data.nextPlayer;

    updateBoardUI();
    updatePlayerDisplay();

    if (data.status === 'finished') {
        gameActive = false;
        document.getElementById('game-status').textContent =
            data.winner === 'r' ? '🔴 Red gagne !' :
            data.winner === 'y' ? '🟡 Yellow gagne !' : 'Match nul !';
        return;
    }

    if (gameMode === 'ia-vs-ia') {
        document.getElementById('next-move-button').classList.remove('hidden');
    } else if (
        (currentPlayer === 'y' && iaTypeYellow !== 'human') ||
        (currentPlayer === 'r' && iaTypeRed !== 'human')
    ) {
        moveInProgress = true;  
        setTimeout(async () => {
            try {
                await playIAMove();
            } finally {
                moveInProgress = false;  
            }
        }, 800);
    }
}

function createBoardUI() {
    const headers = document.getElementById('column-headers');

    headers.innerHTML = '';

    // Column headers
    for (let c = 1; c <= NUM_COLS; c++) {
        const h = document.createElement('div');
        h.className = 'column-header';
        h.textContent = c;
        h.dataset.col = c;
        headers.appendChild(h);
    }

    headers.onclick = (e) => {
        const col = e.target.dataset.col;
        if (!col) return;
        playHumanMove(Number(col));
    };
}

function updateBoardUI() {
    const board = document.getElementById('game-board');

    board.innerHTML = '';

    // Cells
    for (let r = 0; r < NUM_ROWS; r++) {
        for (let c = 0; c < NUM_COLS; c++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            if (currentBoard[r][c] === 'r') cell.classList.add('player-r');
            if (currentBoard[r][c] === 'y') cell.classList.add('player-y');
            board.appendChild(cell);
        }
    }
}

function updatePlayerDisplay() {
    const display = document.getElementById('current-player');
    display.textContent = currentPlayer === 'r' ? '🔴 Red' : '🟡 Yellow';
    
    if (moveInProgress) {
        display.textContent += ' (IA réfléchit...)';
        display.style.opacity = '0.6';
    } else {
        display.style.opacity = '1';
    }
}

// Events
document.getElementById('btn-human-vs-human').onclick = showHumanVsHuman;
document.getElementById('btn-human-vs-ia').onclick = showHumanVsIA;
document.getElementById('btn-ia-vs-ia').onclick = showIAVsIA;

document.getElementById('btn-start-game').onclick = () => {
    if (gameMode === 'human-vs-ia') {
        iaTypeYellow = document.getElementById('ia-opponent-type').value;
    } else {
        iaTypeRed = document.getElementById('ia1-type').value;
        iaTypeYellow = document.getElementById('ia2-type').value;
    }
    showGame();
    initGame();
};

document.getElementById('next-move-button').onclick = () => {
    document.getElementById('next-move-button').classList.add('hidden');
    playIAMove();
};

document.getElementById('btn-back-ia').onclick = showMenu;
document.getElementById('back-to-menu-button').onclick = showMenu;

document.addEventListener('DOMContentLoaded', showMenu);
