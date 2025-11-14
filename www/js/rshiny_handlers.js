let LISTNAME_NCHAR_LIMIT = 0;

const setListLimit = (limit) => {
  LISTNAME_NCHAR_LIMIT = limit;
  return true;
};

const shinyRenameLists = (selectedListNames) => {
  let i;
  if (typeof(selectedListNames) == "object") {
    for (i = 0; i < selectedListNames.length; i++) {
      newListNames[i] = parseNewListName(selectedListNames[i]);
    }
  } else
    newListNames = parseNewListName(selectedListNames);
  updateRenamedListNames(newListNames);
  return true;
};

const parseNewListName = (listName) => {
  return(
    prompt(
      "Rename list ".concat(listName).concat(" (").concat(
        LISTNAME_NCHAR_LIMIT).concat(" characters max):"),
      listName
    )
  )
};

const hideSourceTabs = (prefix) => {
  let i;
  const navbar_li_children = document.getElementById(prefix.concat("_sources_panel")).children;
  for (i = 0; i < navbar_li_children.length; i++){
    navbar_li_children[i].style.display = "none";
  }
  return true;
};

const showSourceTab = (args) => {
  const { prefix, tabPosition } = args,
   navbar_li_children = document.getElementById(prefix.concat("_sources_panel")).children[tabPosition];
  navbar_li_children.style.display = "inline-block";
  return true;
};

const browseUrl = url => {
  window.open(url, "_blank");
};

const pulseUpsetTab = (message) => {
  // Find the tab link containing "UpSet Plot" text and get its parent li element
  const $tab = $('#inputPlots li a:contains("UpSet Plot")').parent('li');

  // Add pulse animation class
  $tab.addClass('tab-pulse-effect');

  // Remove class after animation completes (3 iterations × 0.8s = 2.4s)
  setTimeout(function() {
    $tab.removeClass('tab-pulse-effect');
  }, 2400);

  return true;
};

Shiny.addCustomMessageHandler("handler_setListLimit", setListLimit);
Shiny.addCustomMessageHandler("handler_renameLists", shinyRenameLists);
Shiny.addCustomMessageHandler("handler_hideSourceTabs", hideSourceTabs);
Shiny.addCustomMessageHandler("handler_showSourceTab", showSourceTab);
Shiny.addCustomMessageHandler("handler_browseUrl", browseUrl);
Shiny.addCustomMessageHandler("handler_pulseUpsetTab", pulseUpsetTab);
