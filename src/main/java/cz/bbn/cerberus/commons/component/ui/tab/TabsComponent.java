package cz.bbn.cerberus.commons.component.ui.tab;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.commons.component.ui.dialog.BackButtonDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ObjectWasChangedAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.mainlayout.ui.Navigation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public abstract class TabsComponent<T extends Serializable> extends AppCard {

    private List<TabInternal> tabInnerList;

    private T dto;
    private T originalDto;

    private final boolean hasDto;

    private ObjectWasChangedAction objectWasChangedAction;
    private int activeTab = 0;
    private int selectedTabIndex = 0;

    private Navigation navigation;

    public TabsComponent(String title, List<TabEntry> tabEntryList,
                         EntityNewComponentOperation entityNewComponentOperation) {
        super(entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        hasDto = false;
        init(title, tabEntryList);
    }

    public TabsComponent(String title, List<TabEntry> tabEntryList, int activeTab,
                         Navigation navigation, EntityNewComponentOperation entityNewComponentOperation) {
        super(entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        hasDto = false;
        this.activeTab = activeTab;
        this.navigation = navigation;
        init(title, tabEntryList);
    }

    public TabsComponent(String title, T dto, List<TabEntry> tabEntryList,
                         EntityNewComponentOperation entityNewComponentOperation) {
        super(entityNewComponentOperation);
        hasDto = true;
        this.dto = dto;
        originalDto = SerializationUtils.clone(dto);
        init(title, tabEntryList);
    }

    private void init(String title, List<TabEntry> tabEntryList) {
        H2 header = new H2(title);
        showFooter(true);
        addToHeader(header);

        setSizeFull();
        Tabs tabs = new Tabs();
        this.tabInnerList = getTabMap(tabs, tabEntryList);
        add(tabs);
        tabs.getElement().getStyle().set("display", "grid");
        tabs.setSelectedIndex(selectedTabIndex);

        setUpTabs();
        setUpTabChange(tabs, this.tabInnerList);
    }

    /**
     * each tab has its own save action that shoud handle returning data. but its neccesary to have one "last"
     * call that performs saving. i cant rely on putting this call on any tab, since i dont know order of tabs neither
     * how much of them is there
     *
     * @param button also can be used to save tabs without dto
     */
    public void addSaveButton(Button button) {
        button.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        button.addClassName(RobotFrameworkVariables.SAVE_ITEM_BUTTON_CLASS.getValue());
        addToFooter(button);
    }

    public void addSaveButton() {
        Button save = VaadinComponents.getSubmitButton();
        save.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        save.addClassName(RobotFrameworkVariables.SAVE_ITEM_BUTTON_CLASS.getValue());
        save.setDisableOnClick(true);
        save.addClickListener(e -> {
            save();
            save.setEnabled(true);
        });
        addToFooter(save);
    }

    public void addBackButton() {
        Button backButton = VaadinComponents.getBackButton();
        backButton.addClickListener(buttonClickEvent -> {
                    boolean objectWasChanged = true;
                    if (hasDto) {
                        objectWasChanged = this.objectWasChangedAction != null ?
                                this.objectWasChangedAction.objectWasChanged() : !dto.equals(originalDto);
                    }
                    if (objectWasChanged) {
                        BackButtonDialog backButtonDialog = new BackButtonDialog();
                        backButtonDialog.open();
                    } else {
                        UI.getCurrent().getPage().getHistory().back();
                    }
                }
        );
        addToFooter(backButton);
    }

    public void addToButtonFooter(Component... components) {
        try {
            addToFooter(components);
        } catch (NullPointerException e) {
            log.error("Empty component");
        }
    }

    public void save() {
        for (TabInternal component : tabInnerList) {
            component.getTabSimpleComponent().saveItem();
        }
    }

    private List<TabInternal> getTabMap(Tabs tabs, List<TabEntry> tabEntryList) {
        List<TabInternal> tabInternal = new ArrayList<>();

        for (TabEntry item : tabEntryList) {
            if (item.getPermission() == null || SecurityUtils.hasPermission(item.getPermission())) {
                Tab tab = new Tab(item.getTitle());
                tabs.add(tab);
                TabInternal tabIntItem = new TabInternal(tab, item.getTabSimpleComponent(), item.getTabIndex());
                tabInternal.add(tabIntItem);
                if (item.getTabIndex() != -1 && item.getTabIndex() == activeTab) {
                    selectedTabIndex = tabs.getComponentCount() - 1;
                }
            }
        }

        return tabInternal;
    }

    private void setUpTabs() {
        for (int i = 0, tabInnerListSize = tabInnerList.size(); i < tabInnerListSize; i++) {
            TabInternal item = tabInnerList.get(i);
            add(item.getTabSimpleComponent());

            if (item.getTabIndex() == -1 && i == 0) {
                item.getTab().setSelected(true);
                item.getTabSimpleComponent().setVisible(true);
                continue;
            }

            if (activeTab == item.getTabIndex() && item.getTabIndex() != -1) {
                item.getTab().setSelected(true);
                item.getTabSimpleComponent().setVisible(true);

                Button footerButton = item.getTabSimpleComponent().getFooterButton();
                if (footerButton != null) {
                    cleanFooter();
                    addToFooter(footerButton);
                }
            } else {
                item.getTabSimpleComponent().setVisible(false);
            }
        }
    }

    private void setUpTabChange(Tabs tabs, List<TabInternal> tabList) {
        tabs.addSelectedChangeListener(e -> {
            for (TabInternal item : tabList) {
                if (item.getTab().equals(e.getSelectedTab())) {
                    setUpItem(item);
                    changeTab(item);
                } else {
                    item.getTabSimpleComponent().setVisible(false);
                }
            }
        });
    }

    private void setUpItem(TabInternal item) {
        item.getTab().setSelected(true);
        item.getTabSimpleComponent().setVisible(true);
        item.getTabSimpleComponent().loadTab();
        if (item.getTabIndex() != -1) {
            activeTab = item.getTabIndex();
            if (navigation != null) {
                navigation.setSubmenuByIndex(activeTab);
            }
        }
        Button footerButton = item.getTabSimpleComponent().getFooterButton();
        if (footerButton != null) {
            cleanFooter();
            addToFooter(footerButton);
        }
        boolean clearFooter = item.getTabSimpleComponent().getClearFooter();
        if (clearFooter) {
            cleanFooter();
        }
    }

    public TabInternal getSelectedTab() {
        for (TabInternal item : this.tabInnerList) {
            if(item.getTab().isSelected()){
                return item;
            }
        }
        return null;
    }

    public void changeTab(TabInternal item){
        // implementuje se v momente pokud potrebujeme udelat nejakou dodatecnou operaci pri zmene tabu
    }

    public void setObjectWasChangedAction(ObjectWasChangedAction objectWasChangedAction) {
        this.objectWasChangedAction = objectWasChangedAction;
    }

}
