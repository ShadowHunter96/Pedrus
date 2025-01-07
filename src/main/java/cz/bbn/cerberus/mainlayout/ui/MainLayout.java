package cz.bbn.cerberus.mainlayout.ui;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.applayout.AppLayout;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.dependency.CssImport;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.page.AppShellConfigurator;
import com.vaadin.flow.component.page.Page;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.server.PWA;
import com.vaadin.flow.spring.annotation.UIScope;
import com.vaadin.flow.spring.annotation.VaadinSessionScope;
import com.vaadin.flow.theme.Theme;
import com.vaadin.flow.theme.lumo.Lumo;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dashboard.ui.DashboardView;
import cz.bbn.cerberus.marekdemo.MarekService;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.enums.ApplicationTranslation;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.usermessage.UserMessageService;
import cz.bbn.cerberus.usermessage.ui.components.UserMessageDialog;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.info.BuildProperties;

/**
 * The main view is a top-level placeholder for other views.
 */
@PWA(name = "Cerberus", shortName = "crb")
@JsModule("./styles/shared-styles.js")
@CssImport("./styles/custom-styles.css")
@Theme(variant = Lumo.LIGHT)
@VaadinSessionScope
@UIScope
@PageTitle("Cerberus")
@Slf4j
public class MainLayout extends AppLayout implements AppShellConfigurator {

    private final Button toggleButton = VaadinComponents.getButton(VaadinIcon.MENU.create());

    private final AppEnv appEnv;
    private final BuildProperties buildProperties;
    private final UserMessageService userMessageService;
    private final Navigation navigation;
    private final MainLayoutController mainLayoutController;
    private final UserService userService;
    private final MarekService marekService;

    private com.vaadin.flow.component.html.Section section;

    private Button messagesButton;
    private Span messageCountSpan;
    private int actualCount = 0;

    public MainLayout(AppEnv appEnv, BuildProperties buildProperties, UserMessageService userMessageService,
                      Navigation navigation, MainLayoutController mainLayoutController, UserService userService,
                      HistoryBreadcrumbs historyBreadcrumbs, MarekService marekService) {
        this.appEnv = appEnv;
        this.buildProperties = buildProperties;
        this.userMessageService = userMessageService;
        this.navigation = navigation;
        this.mainLayoutController = mainLayoutController;
        this.userService = userService;
        this.marekService = marekService;
        historyBreadcrumbs.initComponent();
        addToNavbar(true, createHeaderContent());
        addToDrawer(createDrawerContent());
        toggleButton.getElement().setProperty(TextValues.TITLE, Transl.get("Toggle"));

        if (UI.getCurrent() != null) {
            String primColor = appEnv.getStringProperty(AppProperty.ENVIRONMENT_COLOR, "78deg");
            UI.getCurrent().getElement().getStyle().set("--lumo-primary-color-h", primColor + " !important");
        }
    }

    private Component createHeaderContent() {
        Header header = new Header(getTopLayout());
        header.setId(RobotFrameworkVariables.MENU_HEADER_ID.getValue());
        header.setWidthFull();
        header.setId("top-layout");
        return header;
    }

    private HorizontalLayout getTopLayout() {

        Image image = new Image("image-png/logo_solutia", "Logo");
        image.setHeightFull();
        image.setMaxWidth("19.125em");

        image.addClickListener(e -> UI.getCurrent().navigate(DashboardView.ROUTE));
        image.addClassName("cursor-pointer");

        messagesButton = VaadinComponents.getButton(VaadinIcon.BELL.create());
        messagesButton.getElement().setProperty("title", Transl.get("Messages"));
        long messageCount = userMessageService.getUserMessageCount();
        messageCountSpan = new Span(String.valueOf(messageCount));
        refreshMessageCount();

        messagesButton.addClickListener(buttonClickEvent -> {
            UserMessageDialog dialog = new UserMessageDialog(userMessageService, appEnv, this);
            dialog.open();
        });

        navigation.setMainLayout(this);

        Button reloadCashButton = VaadinComponents.getButton(VaadinIcon.REFRESH.create());
        reloadCashButton.getElement().setProperty("title", Transl.get("Reload cash"));
        reloadCashButton.addClickListener(e -> mainLayoutController.reloadCash());
        reloadCashButton.addClassName("top-layout-visibility");

        Button logoutButton = VaadinComponents.getButton(Transl.get("Logout"));
        logoutButton.setIcon(VaadinIcon.SIGN_IN.create());
        logoutButton.addClickListener(buttonClickEvent -> mainLayoutController.logout());
        logoutButton.addClassName("top-layout-visibility");

        HorizontalLayout userInfoLayout = new HorizontalLayout();
        userInfoLayout.setClassName(CssVariables.CURSOR_POINTER.getValue());
        Icon userIcon = VaadinIcon.USER.create();
        userIcon.setClassName("icon-color");

        Span userName = new Span(SecurityUtils.getCurrentUserName());
        userName.setClassName("span-bold");
        userInfoLayout.add(userIcon, userName);
        userInfoLayout.addClickListener(horizontalLayoutClickEvent ->
                mainLayoutController.showUserInfoDialog(this));
        userInfoLayout.addClassName("top-layout-visibility");

        Button marekButton = new Button("Marek test");

        marekButton.addClickListener(e -> marekService.showMe());

        HorizontalLayout versionLayout = new HorizontalLayout();
        versionLayout.setClassName(CssVariables.CURSOR_POINTER.getValue());
        versionLayout.addClickListener(horizontalLayoutClickEvent -> mainLayoutController.showChangelogDialog());

        Span version = new Span(Transl.get("Version")
                .concat(": ")
                .concat(buildProperties.getVersion()));
        version.setClassName("span-bold");
        versionLayout.add(version);
        versionLayout.addClassName("top-layout-visibility");


        HorizontalLayout leftHorizontalLayout = new HorizontalLayout();
        leftHorizontalLayout.add(toggleButton, image);
        leftHorizontalLayout.setAlignItems(FlexComponent.Alignment.CENTER);

        HorizontalLayout rightHorizontalLayout = new HorizontalLayout();
        rightHorizontalLayout.setWidthFull();
        rightHorizontalLayout.setAlignItems(FlexComponent.Alignment.CENTER);
        rightHorizontalLayout.setJustifyContentMode(FlexComponent.JustifyContentMode.END);
        rightHorizontalLayout.add(versionLayout);
        rightHorizontalLayout.add(userInfoLayout);
        rightHorizontalLayout.add(marekButton);
        rightHorizontalLayout.add(messagesButton, messageCountSpan, reloadCashButton);

        rightHorizontalLayout.add(logoutButton, getLanguageComboBox());

        Span envName = new Span(appEnv.getStringProperty(AppProperty.ENVIRONMENT_NAME));
        envName.addClassName("env-name-span");
        envName.addClassName("top-layout-visibility");

        HorizontalLayout centerLayout = new HorizontalLayout();
        centerLayout.add(envName);
        centerLayout.setWidthFull();
        centerLayout.setAlignItems(FlexComponent.Alignment.CENTER);
        centerLayout.setJustifyContentMode(FlexComponent.JustifyContentMode.CENTER);

        HorizontalLayout topHorizontalLayout = new HorizontalLayout();
        topHorizontalLayout.add(leftHorizontalLayout, centerLayout, rightHorizontalLayout);
        topHorizontalLayout.setWidthFull();

        topHorizontalLayout.setMargin(false);

        topHorizontalLayout.setHeightFull();

        addPoolListener();
        return topHorizontalLayout;
    }

    private Component createDrawerContent() {
        section = new com.vaadin.flow.component.html.Section(
                navigation.initNavigation(false), createFooter());

        toggleButton.addClickListener(buttonClickEvent -> navigationRefreshAction(section));

        section.setId("drawer-in");
        return section;
    }

    private void navigationRefreshAction(com.vaadin.flow.component.html.Section section) {
        section.removeAll();
        String style = getElement().getStyle().get("--drawer-width");
        if (getElement().getProperty("overlay").equals("true")
                || (style != null && getElement().getStyle().get("--drawer-width").equals("7em"))) {
            section.add(navigation.initNavigation(false), createFooter());
            setDrawerOpened(true);
            getElement().getStyle().set("--_vaadin-app-layout-drawer-offset-size", "17em");
            getElement().getStyle().set("--drawer-width", "17em");
            UI.getCurrent().getElement().getStyle().set("--overlay-left-padding", "16em");

        } else {
            section.add(navigation.initNavigation(true), createFooter());
            getElement().getStyle().set("--_vaadin-app-layout-drawer-offset-size", "7em");
            getElement().getStyle().set("--drawer-width", "7em");
            UI.getCurrent().getElement().getStyle().set("--overlay-left-padding", "7em");
        }
    }


    private Footer createFooter() {
        Footer layout = new Footer();
        layout.addClassNames("flex", "items-center", "my-s", "px-m", "py-xs");

        VerticalLayout layoutIn = new VerticalLayout();

        Span footer = new Span("Developed by Solutia s.r.o.");

        Span environment = new Span(appEnv.getStringProperty(AppProperty.ENVIRONMENT_NAME));
        environment.addClassName("menu-item-visibility");
        environment.getElement().getStyle().set("word-break", "break-word");

        layoutIn.add(footer);
        layoutIn.add(environment);

        layout.add(layoutIn);

        return layout;
    }

    private ComboBox<ApplicationTranslation> getLanguageComboBox() {
        ComboBox<ApplicationTranslation> comboBox = new ComboBox<>();

        comboBox.setItems(ApplicationTranslation.values());
        comboBox.setValue(UserService.getApplicationTranslation());
        comboBox.setItemLabelGenerator(ApplicationTranslation::getDescription);
        comboBox.setWidth("5em");
        comboBox.addValueChangeListener(event -> {
            userService.setApplicationTranslation(event.getValue());
            UI.getCurrent().getPage().reload();
            log.info("language change to {}", UserService.getApplicationTranslation());
        });
        comboBox.addClassName("top-layout-visibility");

        return comboBox;
    }

    private void addPoolListener() {
        if (UI.getCurrent() != null) {
            UI.getCurrent().setPollInterval(60000);
            UI.getCurrent().addPollListener(pollEvent -> refreshMessageCount());
        }
    }

    public void refreshMessageCount() {
        int count = userMessageService.getUserMessageCount();
        if (count > 0) {
            messageCountSpan.setClassName("message-count-span");
            messageCountSpan.removeAll();
            messageCountSpan.add(String.valueOf(count));
            if (count > actualCount) {
                messagesButton.setClassName("user-message-button-animation");
            } else if (count != actualCount) {
                messagesButton.removeClassName("user-message-button-animation");
            }
        } else {
            messageCountSpan.setClassName("message-count-span-hidden");
            messagesButton.removeClassName("user-message-button-animation");
        }
        actualCount = count;
    }

    @Override
    protected void onAttach(AttachEvent event) {
        super.onAttach(event);
        Page page = UI.getCurrent().getPage();
        page.addBrowserWindowResizeListener(
                e -> navigationRefreshAction(section));
    }
}

