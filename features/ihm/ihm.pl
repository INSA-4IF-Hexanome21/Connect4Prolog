% Obsolète

% :- use_module(library(http/thread_httpd)).
% :- use_module(library(http/http_dispatch)).
% :- use_module(library(http/html_write)).
% %:- use_module(library(pce)).

% % Point d’entrée : démarrer le serveur sur le port 8080
% start :-
%     http_server(http_dispatch, [port(8080)]).

% % Associe l'URL "/" au prédicat index/1
% :- http_handler(root(.), index, []).

% % Associe l'URL "/click" au prédicat on_click/1
% :- http_handler(root(click), on_click, []).

% % Génère la page principale
% index(_Request) :-
%     reply_html_page(
%         title('Mini interface Prolog'),
%         [
%             h1('Ma mini-interface Web'),
%             p('Clique sur le bouton :'),
%             form([action('/click'), method('GET')],
%                  input([type(submit), value('Clique !')]))
%         ]).

% % Ce prédicat réagit au clic
% on_click(_Request) :-
%     reply_html_page(
%         title('Action'),
%         [
%             h2('Merci !'),
%             p('Le prédicat Prolog a bien été exécuté.'),
%             a([href('/')],'Retour')
%         ]).