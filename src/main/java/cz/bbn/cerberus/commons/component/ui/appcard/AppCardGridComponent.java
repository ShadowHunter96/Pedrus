package cz.bbn.cerberus.commons.component.ui.appcard;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H2;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Permission;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;

public class AppCardGridComponent extends AppCard {

    private final String title;
    private final Permission permission;
    private final String route;
    private final ComponentEventListener<ClickEvent<? extends Component>> clickEvent;
    private final List<ComponentEventListener<ClickEvent<? extends Component>>> clickEventList;
    private String buttonTitle;
    private List<String> buttonTitleList;

    public AppCardGridComponent(String title, Permission permission,
                                String buttonTitle, ComponentEventListener<ClickEvent<? extends Component>> clickEvent,
                                EntityNewComponentOperation entityNewComponentOperation,
                                NoteTypeEnum noteTypeEnum, ObjectType objectType) {
        super(entityNewComponentOperation, noteTypeEnum, objectType);
        this.title = title;
        this.permission = permission;
        this.route = null;
        this.buttonTitle = buttonTitle;
        this.clickEvent = clickEvent;
        this.clickEventList = null;
        initCard();
    }

    public AppCardGridComponent(String title, Permission permission, List<String> buttonTitleList,
                                List<ComponentEventListener<ClickEvent<? extends Component>>> clickEventList,
                                EntityNewComponentOperation entityNewComponentOperation,
                                NoteTypeEnum noteTypeEnum, ObjectType objectType) {
        super(entityNewComponentOperation, noteTypeEnum, objectType);
        this.title = title;
        this.permission = permission;
        this.route = null;
        this.buttonTitle = null;
        this.clickEvent = null;
        this.clickEventList = clickEventList;
        this.buttonTitleList = buttonTitleList;
        initCard();
    }

    public AppCardGridComponent(String title, Permission permission,
                                String buttonTitle, ComponentEventListener<ClickEvent<? extends Component>> clickEvent,
                                EntityNewComponentOperation entityNewComponentOperation, NoteTypeEnum noteTypeEnum,
                                ObjectType objectType, Component... components) {
        super(entityNewComponentOperation, noteTypeEnum, objectType);
        this.title = title;
        this.permission = permission;
        this.route = null;
        this.buttonTitle = buttonTitle;
        this.clickEvent = clickEvent;
        this.clickEventList = null;
        initCard(components);
    }

    public AppCardGridComponent(String title, Permission permission, String route, String buttonTitle,
                                EntityNewComponentOperation entityNewComponentOperation) {
        super(entityNewComponentOperation);
        this.title = title;
        this.permission = permission;
        this.route = route;
        this.buttonTitle = buttonTitle;
        this.clickEvent = null;
        this.clickEventList = null;
        initCard();
    }

    public AppCardGridComponent(String title, Permission permission, String route, String buttonTitle,
                                EntityNewComponentOperation entityNewComponentOperation,
                                NoteTypeEnum noteTypeEnum, ObjectType objectType) {
        super(entityNewComponentOperation, noteTypeEnum, objectType);
        this.title = title;
        this.permission = permission;
        this.route = route;
        this.buttonTitle = buttonTitle;
        this.clickEvent = null;
        this.clickEventList = null;
        initCard();
    }

    public AppCardGridComponent(String title,
                                EntityNewComponentOperation entityNewComponentOperation) {
        super(entityNewComponentOperation);
        this.title = title;
        this.permission = null;
        this.route = null;
        this.clickEvent = null;
        this.clickEventList = null;
        initCard();
    }

    public AppCardGridComponent(String title, EntityNewComponentOperation entityNewComponentOperation,
                                NoteTypeEnum noteTypeEnum, ObjectType objectType) {
        super(entityNewComponentOperation, noteTypeEnum, objectType);
        this.title = title;
        this.permission = null;
        this.route = null;
        this.clickEvent = null;
        this.clickEventList = null;
        initCard();
    }

    void initCard(Component... components) {
        Arrays.stream(components).forEach(component -> {
            if (component != null) {
                super.addToFooter(component);
            }
        });
        setContentPadding(true);

        if (StringUtils.isNoneEmpty()) {
            super.addToHeader(new H2(title));
        }

        if (permission != null && SecurityUtils.hasPermission(permission)) {
            if (clickEventList == null) {
                Button addNew = VaadinComponents.getNewButton(buttonTitle);
                if (clickEvent != null) {
                    addNew.addClickListener(clickEvent::onComponentEvent);
                } else {
                    addNew.addClickListener(e -> UI.getCurrent().navigate(route));
                }
                addToFooter(addNew);
                super.showFooter(true);
            } else {
                setNewButtons();
            }
        }
    }

    public void addButtons(Component... components) {
        addToFooter(components);
    }

    private void setNewButtons() {
        if (buttonTitleList != null && clickEventList.size() == buttonTitleList.size()) {
            for (int i = 0; i < clickEventList.size(); i++) {
                Button button = VaadinComponents.getNewButton(buttonTitleList.get(i));
                ComponentEventListener<ClickEvent<? extends Component>> event = clickEventList.get(i);
                button.addClickListener(event::onComponentEvent);
                addToFooter(button);
            }
            super.showFooter(true);
        }
    }

}
