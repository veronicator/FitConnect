const dayOfWeekValues = {
    "MONDAY": 1,
    "TUESDAY": 2,
    "WEDNESDAY": 3,
    "THURSDAY": 4,
    "FRIDAY": 5,
    "SATURDAY": 6
};

/**
 * Create week schedule as daily list
 * @param weekSchedule
 */
function populateTable2(weekSchedule) {
    const tableBody = document.querySelector('#classSchedule2 tbody');

    // Creiamo una singola riga per la tabella
    const row = document.createElement('tr');

    // Per ogni giorno della settimana
    ["MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"].forEach(day => {
        // Creiamo una cella per il giorno corrente
        const cell = document.createElement('td');

        // Troviamo gli orari di classe per il giorno corrente
        const classTimesForDay = weekSchedule.filter(timeSlot => timeSlot.dayOfWeek === day);

        // Ordiniamo gli orari di classe per startTime
        classTimesForDay.sort((a, b) => {
            const startTimeA = parseInt(a.startTime.replace(":", ""));
            const startTimeB = parseInt(b.startTime.replace(":", ""));
            return startTimeA - startTimeB;
        });

        // Creiamo una lista per gli orari di classe di questo giorno
        const ul = document.createElement('ul');

        // Per ogni orario di classe
        classTimesForDay.forEach(timeSlot => {
            // Creiamo un elemento di lista per ogni orario di classe
            const li = document.createElement('li');
            li.textContent = `${timeSlot.startTime} - ${timeSlot.endTime}`;
            ul.appendChild(li);
        });

        // Aggiungiamo la lista alla cella
        cell.appendChild(ul);

        // Aggiungiamo la cella alla riga
        row.appendChild(cell);
    });

    // Aggiungiamo la riga al corpo della tabella
    tableBody.appendChild(row);
}

/**
 * Create full week schedule table
 * @param weekSchedule
 */
function populateTable3(weekSchedule) {
    const tableBody = document.querySelector('#classSchedule3 tbody');

    // Creiamo una matrice vuota per la tabella con un intervallo di ore da 8 a 20
    const tableMatrix = Array(13).fill().map(() => Array(6).fill(''));

    // Popoliamo la matrice con gli orari di classe
    weekSchedule.forEach(timeSlot => {
        const dayIndex = dayOfWeekValues[timeSlot.dayOfWeek] - 1;
        const startTime = parseInt(timeSlot.startTime.split(":")[0]);
        const endTime = parseInt(timeSlot.endTime.split(":")[0]);

        // Popoliamo ogni riga della tabella con gli orari di classe
        for (let i = startTime; i < endTime; i++) {
            tableMatrix[i - 8][dayIndex] = `${timeSlot.startTime} - ${timeSlot.endTime}`;
        }
    });

    // Popoliamo la tabella HTML con i dati dalla matrice
    for (let i = 8; i < 21; i++) {
        const row = document.createElement('tr');
        for (let j = 0; j < 6; j++) {
            const cell = document.createElement('td');
            cell.innerHTML = tableMatrix[i - 8][j];
            row.appendChild(cell);
        }
        tableBody.appendChild(row);
    }
}

function loadTable(weekSchedule){
    const table = document.getElementById('classSchedule2');
    console.log(weekSchedule)
    const cleanedWeekSchedule = weekSchedule.map(timeSlot => ({
        dayOfWeek: timeSlot.dayOfWeek,
        startTime: timeSlot.startTime.split(':').slice(0, 2).join(':'),
        endTime: timeSlot.endTime.split(':').slice(0, 2).join(':')
    }));
    populateTable2(cleanedWeekSchedule);
}