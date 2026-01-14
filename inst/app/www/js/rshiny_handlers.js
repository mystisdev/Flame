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

// Rename modal helpers
const showRenameError = (errorMessage) => {
  const errorDiv = document.querySelector('.rename-error');
  if (errorDiv) {
    errorDiv.textContent = errorMessage;
    errorDiv.style.display = 'block';
  }
  return true;
};

const updateCharCount = (data) => {
  const { count, limit, inputId } = data;
  const element = document.getElementById(inputId);
  if (!element) return false;

  element.textContent = count + '/' + limit + ' characters';

  element.classList.remove('warning', 'error');
  if (count > limit) {
    element.classList.add('error');
  } else if (count > limit * 0.9) {
    element.classList.add('warning');
  }

  // Clear error when typing
  const errorDiv = document.querySelector('.rename-error');
  if (errorDiv) {
    errorDiv.style.display = 'none';
  }

  return true;
};

Shiny.addCustomMessageHandler("handler_showRenameError", showRenameError);
Shiny.addCustomMessageHandler("handler_updateCharCount", updateCharCount);
Shiny.addCustomMessageHandler("handler_browseUrl", browseUrl);
Shiny.addCustomMessageHandler("handler_pulseUpsetTab", pulseUpsetTab);
Shiny.addCustomMessageHandler("handler_pulseTab", pulseTab);
