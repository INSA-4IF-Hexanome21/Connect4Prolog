const gameBoardElement = document.getElementById('game-board');
const columnHeadersElement = document.getElementById('column-headers');
const gameStatusElement = document.getElementById('game-status');
const currentPlayerElement = document.getElementById('current-player');
const resetButton = document.getElementById('reset-button');
const messageArea = document.getElementById('message-area');

let currentBoard = [];
let currentPlayer = 'r'; // 'r' pour rouge (humain), 'a' pour jaune (IA)
let gameActive = true;

const IA_PLAYER = 'a';
const HUMAN_PLAYER = 'r';

const NUM_ROWS = 6;
const NUM_COLS = 7;

// --- Fonctions de communication avec Prolog ---

async function callPrologApi(endpoint, data = {}, method = 'POST') {
    messageArea.textContent = ''; // Effacer messages précédents
    try {
        const response = await fetch(`/api/${endpoint}`, {
            method: method,
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        });
        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(`Erreur HTTP! Status: ${response.status}, Message: ${errorData.error || response.statusText}`);
        }
        return await response.json();
    } catch (error) {
        console.error("Erreur lors de l'appel à l'API Prolog:", error);
        messageArea.textContent = `Erreur: ${error.message}. Consultez la console.`;
        gameActive = false;
        return null;
    }
}

async function initGame() {
    gameActive = true;
    messageArea.textContent = '';
    gameStatusElement.textContent = 'Statut du jeu: En cours';
    gameStatusElement.className = ''; // Supprimer les classes de statut précédentes

    const data = await callPrologApi('init_game', {});
    if (data) {
        currentBoard = data.board;
        currentPlayer = data.player;
        updateBoardUI();
        updatePlayerDisplay();
    }
}

async function makeMove(column) {
    if (!gameActive || currentPlayer !== HUMAN_PLAYER) return; // Seul l'humain peut cliquer

    const moveData = {
        column: column,
        player: currentPlayer,
        currentBoard: currentBoard
    };
    const data = await callPrologApi('move', moveData);

    if (data) {
        currentBoard = data.board;
        currentPlayer = data.player;
        updateBoardUI();
        checkGameEnd(data.status);

        if (gameActive && currentPlayer === IA_PLAYER) {
            // Si c'est le tour de l'IA et le jeu est toujours actif
            setTimeout(callAIMove, 1000); // Petite pause pour simuler la "réflexion"
        }
    }
}

async function callAIMove() {
    if (!gameActive || currentPlayer !== IA_PLAYER) return;

    gameStatusElement.textContent = "Statut du jeu: L'IA réfléchit...";
    gameStatusElement.classList.add('status-ia');

    const aiMoveData = {
        player: IA_PLAYER,
        opponent: HUMAN_PLAYER,
        currentBoard: currentBoard
    };
    const data = await callPrologApi('ai_move', aiMoveData);

    if (data) {
        currentBoard = data.board;
        currentPlayer = data.player; // Devrait être le tour de l'humain maintenant
        updateBoardUI();
        checkGameEnd(data.status);
    }
}

// --- Fonctions d'Actualisation de l'UI ---

function updateBoardUI() {
    columnHeadersElement.innerHTML = '';
    gameBoardElement.innerHTML = '';

    // Créer les en-têtes de colonne cliquables
    for (let c = 0; c < NUM_COLS; c++) {
        const header = document.createElement('div');
        header.classList.add('column-header');
        header.textContent = c + 1;
        header.dataset.column = c + 1;
        header.addEventListener('click', handleColumnClick);
        columnHeadersElement.appendChild(header);
    }

    // Rendre les cellules du plateau
    for (let r = 0; r < NUM_ROWS; r++) {
        for (let c = 0; c < NUM_COLS; c++) {
            const cell = document.createElement('div');
            cell.classList.add('cell');
            const cellValue = currentBoard[r][c];
            if (cellValue === 'r') {
                cell.classList.add('player-r');
            } else if (cellValue === 'a') {
                cell.classList.add('player-a');
            }
            gameBoardElement.appendChild(cell);
        }
    }
}

function updatePlayerDisplay() {
    currentPlayerElement.textContent = currentPlayer === 'r' ? 'Rouge' : 'Jaune';
    currentPlayerElement.style.color = currentPlayer === 'r' ? '#dc3545' : '#ffc107';
}

function checkGameEnd(status) {
    gameStatusElement.classList.remove('status-winner', 'status-draw', 'status-invalid', 'status-ia');
    if (status.winner) {
        gameActive = false;
        if (status.winner === HUMAN_PLAYER) {
            gameStatusElement.textContent = "Statut du jeu: Le joueur Rouge a gagné!";
            gameStatusElement.classList.add('status-winner');
        } else {
            gameStatusElement.textContent = "Statut du jeu: L'IA Jaune a gagné!";
            gameStatusElement.classList.add('status-winner');
        }
    } else if (status === 'draw') {
        gameActive = false;
        gameStatusElement.textContent = "Statut du jeu: Égalité!";
        gameStatusElement.classList.add('status-draw');
    } else if (status === 'invalid_move') {
        messageArea.textContent = "Mouvement invalide. Colonne pleine ou inexistante.";
        messageArea.classList.add('status-invalid');
        // Ne change pas le joueur si le mouvement est invalide
    } else { // 'playing'
        gameStatusElement.textContent = 'Statut du jeu: En cours';
        updatePlayerDisplay();
    }
}

// --- Gestionnaires d'événements ---

function handleColumnClick(event) {
    if (!gameActive || currentPlayer !== HUMAN_PLAYER) {
        if (!gameActive) messageArea.textContent = "La partie est terminée. Cliquez sur 'Recommencer la partie'.";
        else if (currentPlayer !== HUMAN_PLAYER) messageArea.textContent = "Ce n'est pas votre tour!";
        return;
    }

    const column = parseInt(event.target.dataset.column);
    if (!isNaN(column)) {
        makeMove(column);
    }
}

resetButton.addEventListener('click', initGame);

// --- Démarrage du jeu ---
document.addEventListener('DOMContentLoaded', initGame);