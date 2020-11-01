# Summer task #2

## Условие

Требуется написать реализацию игры в крестики-нолики по сети, где соперником будет являться компьютер. Крестики-нолики по сети должны состоять из сервера и клиента. Игрок будет подключаться к серверу, выбирать размер поля, а затем случайным образом будет выбраться сторона игрока (крестик или нолик) и начинаться игра.

### Базовые требования к заданию

Необходимо успешное выполнение сценария:

1. Запуск сервера
2. Подключение клиента к указанному IP-адресу
3. Запуск сессии, выбор размеров поля
4. Старт игры против компьютера
5. Отображение результата, выбор начать заново или закрыть сессию

### Базовые требования к серверу

Работа по локальной сети с минимум 100 клиентами одновременно (тест на количество соединений будет правильным решением). Сервер выступает в качестве хоста каждой из сессий и соперником для игрока.

### Базовые требования к клиенту

Возможность интерактивного взаимодействия с доской (например, выбор точки при помощи клавиш) в виде TUI или GUI.

### Бонусы

Любые дополнительные возможности будут приветствоваться. Это может быть, например: лог игр, возможность быть зрителем матча, игра по WAN, auto discovery, дополнительный ход, игра с другим человеком, умный бот.

### Рекомендации

* Для работы с сетью рекомендуется использовать [Servant](https://hackage.haskell.org/package/servant).
* Для реализации TUI можно использовать библиотеку [Brick](https://hackage.haskell.org/package/brick).

## Запуск

* `stack build` для сборки проекта
* `stack run -- server` для запуска сервера
* `stack run -- client` для запуска клиента

Проект можно собирать как на UNIX-системах, так и на Windows.