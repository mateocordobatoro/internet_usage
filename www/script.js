document.addEventListener("click", function(event) {
    let floatContainer = document.querySelector(".float-container");

    if (!floatContainer.contains(event.target)) {
        console.log("Click fuera del float-container");
        // Aquí puedes agregar la acción para cuando se haga clic fuera
    } else {
        console.log("Click dentro del float-container");
        // Aquí puedes agregar la acción para cuando se haga clic dentro
    }
});
