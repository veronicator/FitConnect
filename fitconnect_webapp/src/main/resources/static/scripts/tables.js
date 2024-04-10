/**
 * Create week schedule as daily list
 * @param weekSchedule
 */
function populateTable(weekSchedule) {
    const tableBody = document.querySelector('#classSchedule tbody');

    // Creiamo una singola riga per la tabella
    const row = document.createElement('tr');

    // Per ogni giorno della settimana
    ["MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"].forEach(day => {
        // Creiamo una cella per il giorno corrente
        const cell = document.createElement('td');
        cell.setAttribute("class", "text-center")

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

        // Aggiungiamo la riga al corpo della tabella
        tableBody.appendChild(row);

    });

}

function loadTable(weekSchedule) {
    const table = document.getElementById('classSchedule');
    //const weekSchedule = table.getAttribute('data-weekSchedule');
    const cleanedWeekSchedule = weekSchedule.map(timeSlot => ({
        dayOfWeek: timeSlot.dayOfWeek,
        startTime: timeSlot.startTime.split(':').slice(0, 2).join(':'),
        endTime: timeSlot.endTime.split(':').slice(0, 2).join(':')
    }));
    console.log(cleanedWeekSchedule);
    populateTable(cleanedWeekSchedule);
}