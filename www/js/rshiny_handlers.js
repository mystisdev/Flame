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

const hideSourceTabs = (prefix, retryCount = 0) => {
  const elementId = prefix.concat("_sources_panel");
  const element = document.getElementById(elementId);
  if (!element) {
    // Element not ready yet (DOM not updated), retry after a short delay
    if (retryCount < 10) {
      setTimeout(() => hideSourceTabs(prefix, retryCount + 1), 100);
      return false;
    }
    return false;
  }
  // Find the nav tabs (ul element) inside the sources panel
  const navTabs = element.querySelector("ul.nav-tabs");
  if (!navTabs) {
    // Try retrying in case the inner structure isn't ready
    if (retryCount < 10) {
      setTimeout(() => hideSourceTabs(prefix, retryCount + 1), 100);
      return false;
    }
    return false;
  }
  const navbar_li_children = navTabs.children;
  for (let i = 0; i < navbar_li_children.length; i++){
    navbar_li_children[i].style.display = "none";
  }
  return true;
};

const showSourceTab = (args, retryCount = 0) => {
  const { prefix, tabPosition } = args;
  const elementId = prefix.concat("_sources_panel");
  const element = document.getElementById(elementId);
  if (!element) {
    if (retryCount < 10) {
      setTimeout(() => showSourceTab(args, retryCount + 1), 100);
      return false;
    }
    return false;
  }
  const navTabs = element.querySelector("ul.nav-tabs");
  if (!navTabs) {
    if (retryCount < 10) {
      setTimeout(() => showSourceTab(args, retryCount + 1), 100);
      return false;
    }
    return false;
  }
  const tabLi = navTabs.children[tabPosition];
  if (tabLi) {
    tabLi.style.display = "inline-block";
  }
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

  // Remove class after animation completes (3 iterations x 0.8s = 2.4s)
  setTimeout(function() {
    $tab.removeClass('tab-pulse-effect');
  }, 2400);

  return true;
};

const pulseTab = (tabId, retryCount = 0) => {
  // Add 500ms initial delay to ensure modal is removed first
  const initialDelay = retryCount === 0 ? 500 : 0;

  setTimeout(() => {
    // Find tab by data-value attribute
    let $tab = $('#toolTabsPanel li a[data-value="' + tabId + '"]').parent('li');

    // Fallback: try broader search
    if ($tab.length === 0) {
      $tab = $('ul.nav-tabs li a[data-value="' + tabId + '"]').parent('li');
    }

    if ($tab.length === 0) {
      // Tab might not be rendered yet, retry
      if (retryCount < 10) {
        setTimeout(() => pulseTab(tabId, retryCount + 1), 100);
        return false;
      }
      return false;
    }

    // Apply pulse to the <a> element inside (has visible background styling)
    const $anchor = $tab.find('a').first();
    $anchor.addClass('tab-pulse-effect');

    // Remove class after animation completes (3 iterations x 0.8s = 2.4s)
    setTimeout(function() {
      $anchor.removeClass('tab-pulse-effect');
    }, 2400);
  }, initialDelay);

  return true;
};

Shiny.addCustomMessageHandler("handler_setListLimit", setListLimit);
Shiny.addCustomMessageHandler("handler_renameLists", shinyRenameLists);
Shiny.addCustomMessageHandler("handler_hideSourceTabs", hideSourceTabs);
Shiny.addCustomMessageHandler("handler_showSourceTab", showSourceTab);
Shiny.addCustomMessageHandler("handler_browseUrl", browseUrl);
Shiny.addCustomMessageHandler("handler_pulseUpsetTab", pulseUpsetTab);
Shiny.addCustomMessageHandler("handler_pulseTab", pulseTab);
