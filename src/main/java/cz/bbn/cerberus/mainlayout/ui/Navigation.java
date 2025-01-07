package cz.bbn.cerberus.mainlayout.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.contextmenu.SubMenu;
import com.vaadin.flow.component.html.Nav;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.RouterLink;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.menubar.MenuBar;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.enums.ApplicationTranslation;
import lombok.SneakyThrows;
import org.springframework.boot.info.BuildProperties;
import org.springframework.web.context.annotation.SessionScope;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

@org.springframework.stereotype.Component
@SessionScope
public class Navigation {

    private final MainLayoutController mainLayoutController;
    private final BuildProperties buildProperties;

    private MainLayout mainLayout;
    private MenuBar menuBar;
    private MenuBar mobileMenuBar;
    private MenuItem selectedMenuItem;
    private SubMenuItem selectedSubmenuItem;
    private boolean onlyIcon;

    public Navigation(MainLayoutController mainLayoutController, BuildProperties buildProperties) {
        this.mainLayoutController = mainLayoutController;
        this.buildProperties = buildProperties;
    }

    public Nav initNavigation(boolean onlyIcon) {
        Nav navigation = new Nav();
        navigation.addClassName("drawer-navigation");
        navigation.addClassNames("border-b", "border-contrast-10", "flex-grow", "overflow-auto");
        navigation.getElement().setAttribute("aria-labelledby", "views");

        menuBar = new MenuBar();
        menuBar.setId(RobotFrameworkVariables.MENU_ITEMS_ID.getValue());
        menuBar.addClassName("menu-items");
        menuBar.setOpenOnHover(true);
        menuBar.getElement().setAttribute("theme", "menu-vertical");
        mobileMenuBar = new MenuBar();
        mobileMenuBar.setId(RobotFrameworkVariables.MENU_MOBILE_ITEMS_ID.getValue());
        mobileMenuBar.addClassName("menu-items");
        mobileMenuBar.addClassName("menu-item-visibility");
        mobileMenuBar.getElement().getStyle().set("padding-top", "0px");
        mobileMenuBar.setOpenOnHover(true);
        mobileMenuBar.getElement().setAttribute("theme", "menu-vertical");
        refreshMenu(onlyIcon);
        navigation.add(menuBar, mobileMenuBar);
        this.onlyIcon = onlyIcon;

        return navigation;
    }

    public void checkUrlAndChangeMenuItem(BeforeEnterEvent event) {
        Class<AppView> actualItem = (Class<AppView>) event.getNavigationTarget();
        if (selectedMenuItem != null && !actualItem.equals(selectedMenuItem.getView())) {
            findActualMenuItemFromAppViewClass(
                    actualItem, event.getRouteParameters().get("___url_parameter").orElse(""));
        } else if (selectedMenuItem == null) {
            selectedMenuItem = MenuItem.DASHBOARD;
        }
        refreshMenu(onlyIcon);
    }

    public void setMainLayout(MainLayout mainLayout) {
        this.mainLayout = mainLayout;
    }

    private void findActualMenuItemFromAppViewClass(Class<AppView> projectView, String param) {
        MenuItem menuItem = Arrays.stream(MenuItem.values()).filter(actualMenuItem ->
                actualMenuItem.getViewList() != null && Arrays.stream(actualMenuItem.getViewList()).anyMatch(
                        aClass -> Objects.equals(projectView.getName(), aClass.getName()))).findFirst().orElse(null);

        if (menuItem != null) {
            selectedMenuItem = menuItem;
        } else {
            List<MenuItem> actualList = Arrays.stream(MenuItem.values()).filter(
                    menuItem1 -> menuItem1.getSubMenuItems().length != 0).toList();

            actualList.forEach(menuItem1 -> {
                boolean match = Arrays.stream(menuItem1.getSubMenuItems())
                        .anyMatch(subMenuItem -> Objects.equals(subMenuItem.getView().getName(), projectView.getName())
                                || Arrays.stream(subMenuItem.getViewArray())
                                .anyMatch(aClass -> Objects.equals(aClass.getName(), projectView.getName())));
                if (match) {
                    selectedMenuItem = menuItem1;
                    selectedSubmenuItem =
                            Arrays.stream(menuItem1.getSubMenuItems())
                                    .filter(subMenuItem ->
                                            (subMenuItem.getTabIndex() == null &&
                                                    Objects.equals(subMenuItem.getView()
                                                            .getName(), projectView.getName())) ||
                                                    (subMenuItem.getTabIndex() != null &&
                                                            subMenuItem
                                                                    .getTabIndex()
                                                                    .equals(Integer.valueOf(param))) ||
                                                    subMenuItem.getTabIndex() == null &&
                                                            Arrays.stream(subMenuItem.getViewArray()).anyMatch(
                                                                    aClass -> Objects.equals(
                                                                            aClass.getName(), projectView.getName())))
                                    .findFirst().orElse(null);
                }
            });
        }
    }

    public void setSubmenuByIndex(Integer index) {
        if (selectedMenuItem.getSubMenuItems().length > 0) {
            selectedSubmenuItem = Arrays.stream(selectedMenuItem
                            .getSubMenuItems())
                    .filter(subMenuItem -> index.equals(subMenuItem.getTabIndex())).findFirst().orElse(null);
            refreshMenu(onlyIcon);
        }
    }

    private void addItem(MenuItem menuItemInfo, boolean onlyIcon) {
        com.vaadin.flow.component.contextmenu.MenuItem mainMenuItem =
                menuBar.addItem(getRouteLink(Transl.get(menuItemInfo.getText()), menuItemInfo.getIcon(), onlyIcon));
        mainMenuItem.setId(menuItemInfo.name());

        mainMenuItem.addClickListener(menuItemClickEvent -> {
            if (menuItemInfo.getSubMenuItems().length < 1) {
                UI.getCurrent().navigate(getMainMenuUrl(menuItemInfo));
            } else {
                UI.getCurrent().navigate(getSubmenuUrl(menuItemInfo.getSubMenuItems()[0]));
            }
        });

        if (menuItemInfo == selectedMenuItem) {
            if ((menuItemInfo.getSubMenuItems().length < 1) || onlyIcon) {
                mainMenuItem.addThemeNames("selected");

            }
            if (!onlyIcon) {
                Arrays.stream(menuItemInfo.getSubMenuItems()).forEach(subMenuItem -> {
                    if (SecurityUtils.isAccessGranted(subMenuItem.getView())) {
                        com.vaadin.flow.component.contextmenu.MenuItem actualSubMenu = menuBar.addItem(getRouteLink(
                                Transl.get(subMenuItem.getText()), subMenuItem.getIcon(), onlyIcon));
                        actualSubMenu.setVisible(true);
                        actualSubMenu.setId(subMenuItem.name());
                        actualSubMenu.addThemeNames("submenu");
                        if (selectedSubmenuItem != null && selectedSubmenuItem == subMenuItem) {
                            actualSubMenu.addThemeNames("selected");
                        }
                        actualSubMenu.addClickListener(menuItemClickEvent ->
                                UI.getCurrent().navigate(getSubmenuUrl(subMenuItem)));
                    }
                });
            }
        }

        if (menuItemInfo.getSubMenuItems().length > 0) {
            SubMenu subMenu = mainMenuItem.getSubMenu();
            Arrays.stream(menuItemInfo.getSubMenuItems()).forEach(subMenuItem -> {
                if (SecurityUtils.isAccessGranted(subMenuItem.getView())) {
                    com.vaadin.flow.component.contextmenu.MenuItem actualMenuItem = subMenu.addItem(
                            getRouteLink(Transl.get(subMenuItem.getText()), subMenuItem.getIcon(), false));
                    actualMenuItem.addThemeNames("selected");
                    actualMenuItem.addClickListener(
                            menuItemClickEvent -> UI.getCurrent().navigate(getSubmenuUrl(subMenuItem)));
                }
            });
        }


        if (!onlyIcon && menuItemInfo.getAddSeparator()) {
            com.vaadin.flow.component.contextmenu.MenuItem separatorMenuItem = menuBar.addItem("");
            separatorMenuItem.addThemeNames("separator");
        }

    }

    private RouterLink getRouteLink(String translatedText, VaadinIcon vaadinIcon, boolean onlyIcon) {
        RouterLink link = new RouterLink();
        link.setClassName("menu-link");
        Span icon = new Span();
        icon.setTitle(translatedText);

        if (vaadinIcon != null) {
            icon.add(vaadinIcon.create());
        }

        link.add(icon);
        if (!onlyIcon) {
            Span text = new Span("   ".concat(translatedText));
            link.add(text);
        }
        return link;
    }

    @SneakyThrows
    private String getMainMenuUrl(MenuItem menuItem) {
        final Field field = Class.forName(menuItem.getView().getName()).getDeclaredField("ROUTE");
        return String.valueOf(field.get(null));
    }

    @SneakyThrows
    private String getSubmenuUrl(SubMenuItem item) {
        final Field field = Class.forName(item.getView().getName()).getDeclaredField("ROUTE");
        String param = item.getTabIndex() != null ? "/".concat(String.valueOf(item.getTabIndex())) : "";
        return String.valueOf(field.get(null)).concat(param);
    }

    private void refreshMenu(boolean onlyIcon) {
        menuBar.removeAll();
        for (MenuItem menuItem : MenuItem.values()) {
            if (SecurityUtils.isAccessGranted(menuItem)) {
                addItem(menuItem, onlyIcon);
            }
        }

        mobileMenuBar.removeAll();
        if (!onlyIcon) {
            com.vaadin.flow.component.contextmenu.MenuItem separatorMenuItem = mobileMenuBar.addItem("");
            separatorMenuItem.addThemeNames("separator");
        }
        mobileMenuBar.addItem(getNoRouteItemSpan(Transl.get("User info"), VaadinIcon.USER, onlyIcon),
                e -> mainLayoutController.showUserInfoDialog(mainLayout));
        if (SecurityUtils.hasPermission(Permission.DASHBOARD)) {
            mobileMenuBar.addItem(getNoRouteItemSpan(
                    Transl.get("Version").concat(": ").concat(buildProperties.getVersion()),
                    VaadinIcon.TOOLS, onlyIcon), e -> mainLayoutController.showChangelogDialog());
        }

        if (SecurityUtils.hasPermission(Permission.DASHBOARD)) {
            mobileMenuBar.addItem(getNoRouteItemSpan(Transl.get("Reload cash"), VaadinIcon.REFRESH, onlyIcon),
                    e -> mainLayoutController.reloadCash());
        }
        com.vaadin.flow.component.contextmenu.MenuItem languageItem =
                mobileMenuBar.addItem(getNoRouteItemSpan(Transl.get("Language"), VaadinIcon.COMMENTS_O, onlyIcon));
        SubMenu languageSubMenu = languageItem.getSubMenu();
        languageSubMenu.addItem(getNoRouteItemSpan(Transl.get("English"), null, false),
                e -> mainLayoutController.changeLanguage(ApplicationTranslation.EN));
        languageSubMenu.addItem(getNoRouteItemSpan(Transl.get("Czech"), null, false),
                e -> mainLayoutController.changeLanguage(ApplicationTranslation.CS));
        mobileMenuBar.addItem(getNoRouteItemSpan(Transl.get("Logout"), VaadinIcon.SIGN_IN, onlyIcon),
                e -> mainLayoutController.logout());
    }

    private Span getNoRouteItemSpan(String title, VaadinIcon vaadinIcon, boolean onlyIcon) {
        Span icon = new Span();
        icon.setClassName("menu-link");
        icon.setTitle(title);

        if (vaadinIcon != null) {
            icon.add(vaadinIcon.create());
        }
        if (!onlyIcon) {
            Span text = new Span("   ".concat(title));
            icon.add(text);
        }
        return icon;
    }
}
