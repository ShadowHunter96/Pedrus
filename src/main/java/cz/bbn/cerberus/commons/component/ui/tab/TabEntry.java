package cz.bbn.cerberus.commons.component.ui.tab;

import cz.bbn.cerberus.permission.Permission;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TabEntry {

    private TabSimpleComponent tabSimpleComponent;
    private String title;
    private Permission permission;
    private int tabIndex;

    public TabEntry(String title, TabSimpleComponent tabSimpleComponent) {
        this.title = title;
        this.tabSimpleComponent = tabSimpleComponent;
        this.tabIndex = -1;
    }

    public TabEntry(String title, TabSimpleComponent tabSimpleComponent, Permission permission) {
        this.tabSimpleComponent = tabSimpleComponent;
        this.title = title;
        this.permission = permission;
        this.tabIndex = -1;
    }

    public TabEntry(String title, TabSimpleComponent tabSimpleComponent, Permission permission, int tabIndex) {
        this.tabSimpleComponent = tabSimpleComponent;
        this.title = title;
        this.permission = permission;
        this.tabIndex = tabIndex;
    }
}
