package cz.bbn.cerberus.commons.component.ui.breadcrump;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.contextmenu.MenuItem;
import com.vaadin.flow.component.contextmenu.SubMenu;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.menubar.MenuBar;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dashboard.ui.DashboardView;
import cz.bbn.cerberus.mainlayout.ui.SubMenuItem;
import cz.bbn.cerberus.translation.Transl;
import org.springframework.web.context.annotation.SessionScope;



@org.springframework.stereotype.Component
@SessionScope
public class HistoryBreadcrumbs {

    private final int historyItemMaxCount;
    private HorizontalLayout breadcrumbs;
    private Breadcrumb lastBreadcrumb;
    private MenuBar menuBar;
    private MenuItem menuItem;

    public HistoryBreadcrumbs(AppEnv appEnv) {
        historyItemMaxCount = appEnv.getHistoryItemCount();
    }

    public void initComponent(){
        breadcrumbs = new HorizontalLayout();
        menuBar = new MenuBar();
        menuBar.addThemeNames("breadcrumb-button");
        menuBar.addClassName("breadcrumb-button");
        menuItem = createIconItem(menuBar, VaadinIcon.ANGLE_DOWN);
        breadcrumbs.add(menuBar);
        if(lastBreadcrumb != null){
            breadcrumbs.add(createLink(lastBreadcrumb));

        }else{
            breadcrumbs.add(createDashboardLink());
        }
        breadcrumbs.setClassName("breadcrumb-layout");
    }

    private MenuItem createIconItem(MenuBar menu, VaadinIcon iconName) {
        Icon icon = new Icon(iconName);
        return menu.addItem(icon);
    }

    public void reload(BeforeEnterEvent event){
        if(breadcrumbs.getChildren().count() > historyItemMaxCount){
            breadcrumbs.remove(breadcrumbs.getComponentAt(1));
            menuItem.getSubMenu().remove(menuItem.getSubMenu().getItems().get(0));
        }
        String path = event.getLocation().getPath();
        if(!path.isEmpty() && (lastBreadcrumb == null || !lastBreadcrumb.getHref().equals(path))) {
            breadcrumbs.add(createLink(event, path));

        }else if(path.isEmpty()){
            breadcrumbs.add(createDashboardLink());
        }
    }

    private Anchor createLink(BeforeEnterEvent event, String path){
        Anchor link;
        Class<AppView> clazz = (Class<AppView>) event.getNavigationTarget();
        SubMenuItem subMenuItem = SubMenuItem.findSubmenuMenuItemByClass(clazz);
        VaadinIcon icon = SubMenuItem.getIconByClass(clazz);
        if(subMenuItem != null){
            link = createLink(new Breadcrumb(path, Transl.get(subMenuItem.getText()), icon));
        }else{
            link = createLink(new Breadcrumb(path, Transl.get(event.getLocation().getFirstSegment()), icon));
        }
        return link;
    }

    private Anchor createLink(Breadcrumb breadcrumb){
        Anchor link;
        SubMenu moveSubMenu = menuItem.getSubMenu();
        if(breadcrumb.getIcon() != null){
            Icon icon = breadcrumb.getIcon().create();
            icon.addClassName("breadcrumb-text-icon");
            link = new Anchor(breadcrumb.getHref(), icon, new Label(breadcrumb.getText()));
            HorizontalLayout horizontalLayout = new HorizontalLayout();
            Icon menuIcon = breadcrumb.getIcon().create();
            icon.addClassName("breadcrumb-text-icon");
            horizontalLayout.add(menuIcon, new Label(breadcrumb.getText()));
            horizontalLayout.setPadding(false);
            horizontalLayout.setMargin(false);
            moveSubMenu.addItem(horizontalLayout)
                    .addClickListener(menuItemClickEvent ->
                    UI.getCurrent().access(() -> UI.getCurrent().navigate(link.getHref())));
        }else{
            link = new Anchor(breadcrumb.getHref(), new Label(breadcrumb.getText()));
            moveSubMenu.addItem(new Label(breadcrumb.getFullText()))
                    .addClickListener(menuItemClickEvent ->
                    UI.getCurrent().access(() -> UI.getCurrent().navigate(link.getHref())));
        }
        link.setClassName("c-breadcrumbs__item c-item_default");
        link.getElement().setProperty(TextValues.TITLE, breadcrumb.getTitle());
        Icon splitterIcon = VaadinIcon.CHEVRON_RIGHT.create();
        splitterIcon.addClassName("breadcrumb-icon");
        link.add(splitterIcon);
        lastBreadcrumb = breadcrumb;
        return link;
    }

    private Anchor createDashboardLink(){
        return createLink(new Breadcrumb(DashboardView.ROUTE, Transl.get(cz.bbn.cerberus.mainlayout.ui.MenuItem.DASHBOARD.getText()),
                        cz.bbn.cerberus.mainlayout.ui.MenuItem.DASHBOARD.getIcon()));
    }

    public void refreshLastBreadcrumb(String url){
        lastBreadcrumb.setHref(url);
        int lastItem = breadcrumbs.getChildren().toList().size() - 1;
        int subMenuLastItem = (int) menuItem.getSubMenu().getChildren().count() - 1;
        breadcrumbs.remove(breadcrumbs.getComponentAt(lastItem));
        menuItem.getSubMenu().remove(menuItem.getSubMenu().getChildren().toList().get(subMenuLastItem));
        breadcrumbs.add(createLink(lastBreadcrumb));
    }

    public void refreshTextBreadcrumb(String text){
        lastBreadcrumb.setText(text);
        lastBreadcrumb.setTitle(text + " -> " + lastBreadcrumb.getHref());
        int lastItem = breadcrumbs.getChildren().toList().size() - 1;
        int subMenuLastItem = (int) menuItem.getSubMenu().getChildren().count() - 1;
        breadcrumbs.remove(breadcrumbs.getComponentAt(lastItem));
        menuItem.getSubMenu().remove(menuItem.getSubMenu().getChildren().toList().get(subMenuLastItem));
        breadcrumbs.add(createLink(lastBreadcrumb));
    }

    public HorizontalLayout getBreadcrumbs() {
        return breadcrumbs;
    }
}
