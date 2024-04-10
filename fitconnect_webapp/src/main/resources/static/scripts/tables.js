/**
 * Create week schedule as daily list
 * @param weekSchedule
 */
function populateTable(weekSchedule) {
    const tableBody = document.querySelector('#classSchedule tbody');

    const row = document.createElement('tr');

    // for every day
    ["MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"].forEach(day => {
        // cell for the current day
        const cell = document.createElement('td');
        cell.setAttribute("class", "text-center")

        // Search classes for the respective day
        const classTimesForDay = weekSchedule.filter(timeSlot => timeSlot.dayOfWeek === day);

        // sort classes by startTime
        classTimesForDay.sort((a, b) => {
            const startTimeA = parseInt(a.startTime.replace(":", ""));
            const startTimeB = parseInt(b.startTime.replace(":", ""));
            return startTimeA - startTimeB;
        });

        const ul = document.createElement('ul');

        classTimesForDay.forEach(timeSlot => {
            // Creiamo un elemento di lista per ogni orario di classe
            const li = document.createElement('li');
            li.textContent = `${timeSlot.startTime} - ${timeSlot.endTime}`;
            ul.appendChild(li);
        });

        cell.appendChild(ul);

        row.appendChild(cell);

        tableBody.appendChild(row);

    });

}

function loadTable(weekSchedule) {
    const table = document.getElementById('classSchedule');
    const cleanedWeekSchedule = weekSchedule.map(timeSlot => ({
        dayOfWeek: timeSlot.dayOfWeek,
        startTime: timeSlot.startTime.split(':').slice(0, 2).join(':'),
        endTime: timeSlot.endTime.split(':').slice(0, 2).join(':')
    }));
    console.log(cleanedWeekSchedule);
    populateTable(cleanedWeekSchedule);
}