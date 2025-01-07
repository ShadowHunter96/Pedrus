package cz.bbn.cerberus.commons.component.ui.appcard;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.contextmenu.MenuItem;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.CenteredHorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.menubar.MenuBar;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideBarComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideTabItem;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class AppCard extends VerticalLayout {

    private final SlideTabItem[] slideTabItems = new SlideTabItem[3];
    private final Div topBar = new Div();
    private final HorizontalLayout header = new HorizontalLayout();
    private final VerticalLayout content = new VerticalLayout();
    private final VerticalLayout footer = new VerticalLayout();
    private final CenteredHorizontalLayout centeredHorizontalLayout = new CenteredHorizontalLayout();
    private final NoteTypeEnum noteTypeEnum;
    private final ObjectType objectType;

    private final EntityNewComponentOperation entityNewComponentOperation;

    public AppCard(EntityNewComponentOperation entityNewComponentOperation) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.noteTypeEnum = null;
        this.objectType = null;
        initAppCard();
    }

    public AppCard(EntityNewComponentOperation entityNewComponentOperation,
                   NoteTypeEnum noteTypeEnum, ObjectType objectType) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.noteTypeEnum = noteTypeEnum;
        this.objectType = objectType;
        initAppCard();
    }

    private void initAppCard() {
        this.addClassName("card-content");
        this.setSizeFull();
        this.setPadding(false);
        this.setMargin(false);
        topBar.setWidthFull();
        topBar.setHeight("0px");
        content.setSizeFull();
        content.getElement().getStyle().set("overflow", "auto");
        content.setMargin(false);
        content.setPadding(false);
        footer.setWidthFull();
        footer.addClassName("card-footer");
        footer.addClassName("card-bottom-menu-radius");
        header.setMargin(false);
        header.setPadding(false);
        header.setWidthFull();
        this.addComponentAtIndex(0, topBar);
        this.addComponentAtIndex(1, header);
        this.addComponentAtIndex(2, content);
        this.addComponentAtIndex(3, footer);
        footer.add(centeredHorizontalLayout);
        footer.setVisible(false);
    }

    public void showFooter(boolean showFooter) {
        footer.setVisible(showFooter);
    }

    public void setContentPadding(boolean value) {
        content.setPadding(value);
    }

    public VerticalLayout getContent() {
        return content;
    }

    private void addToTopBar(Component... components) {
        topBar.add(components);
    }

    public void addNewEntitySlideTab(NewEntityButtonsComponent newEntityButtonsComponent) {
        if (newEntityButtonsComponent.getButtonCount() != 0) {
            slideTabItems[1] = new SlideTabItem("Entity", Transl.get("New entity"), newEntityButtonsComponent);
        }
    }

    public void addEventSlideTab(TaskSlideTabComponent taskSlideTabComponent) {
        slideTabItems[0] = new SlideTabItem(
                "Event", Transl.get("Events"), taskSlideTabComponent,
                taskSlideTabComponent.getTaskIndicator());
    }

    public void addNoteSlideTab(NoteComponent noteComponent, CountIntIndicator indicator) {
        slideTabItems[2] = new SlideTabItem("Note", Transl.get("Notes"), noteComponent, indicator);
    }

    private void addDefaultNewEntitySlideTab(EntityNewComponentOperation entityNewComponentOperation) {
        List<SlideTabItem> slideTabItemList = new ArrayList<>();

        if (NewEntityButtonsComponent.hasAllPermission()) {
            NewEntityButtonsComponent newEntityButtonsComponent =
                    new NewEntityButtonsComponent(entityNewComponentOperation);
            if (newEntityButtonsComponent.getButtonCount() != 0) {
                slideTabItemList.add(new SlideTabItem("Entity", Transl.get("New entity"), newEntityButtonsComponent));
            }
        }
        this.addToTopBar(new SlideBarComponent(slideTabItemList));
    }

    public void addToHeader(Component... components) {
        this.header.add(components);
    }

    public void addToFooter(Component... components) {
        this.centeredHorizontalLayout.add(components);
    }

    public void addToContent(Component... components) {
        content.add(components);
    }

    public void cleanFooter() {
        this.centeredHorizontalLayout.removeAll();
    }

    @Override
    public void add(Component... components) {
        content.add(components);
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        List<SlideTabItem> slideTabItemList = new ArrayList<>();
        int numberOfElements = 0;
        if (objectType != null && slideTabItems[0] == null) {
            addEventSlideTab(new TaskSlideTabComponent(entityNewComponentOperation.getEventComponentOperation(),
                    entityNewComponentOperation.getAppEnv(), objectType, null, null,
                    entityNewComponentOperation.getListService()));
        }
        if (noteTypeEnum != null && slideTabItems[2] == null) {

            Set<String> permissionSet = new HashSet<>();
            permissionSet.add(noteTypeEnum.getViewPerm().name());

            NoteFilterDto filterDto = new NoteFilterDto();
            filterDto.setNoteTypeEnum(noteTypeEnum);
            filterDto.setPermission(permissionSet);

            CountIntIndicator noteIndicator = new CountIntIndicator(
                    entityNewComponentOperation.getNoteCount(filterDto));
            addNoteSlideTab(new NoteComponent(entityNewComponentOperation.getNoteComponentService(),
                    new Checkbox(Transl.get("Show also archived")), null, entityNewComponentOperation.getAppEnv(),
                    noteTypeEnum, noteTypeEnum.getViewPerm(), noteTypeEnum.getEditPerm(), true,
                    false, noteIndicator), noteIndicator);
        }
        for (int i = 0; i < slideTabItems.length; i++) {
            if (slideTabItems[i] != null) {
                numberOfElements++;
                if (!slideTabItems[i].getComponent().getChildren().toList().isEmpty()) {
                    slideTabItemList.add(slideTabItems[i]);
                }
            }
        }

        if (numberOfElements == 0 && entityNewComponentOperation != null) {
            addDefaultNewEntitySlideTab(entityNewComponentOperation);
        } else {
            addToTopBar(new SlideBarComponent(slideTabItemList));
        }
    }

    public void showSubjectsMenu(List<SubjectDto> list) {
        MenuBar menuBar = new MenuBar();
        menuBar.setThemeName("menu-bar-as-button");
        menuBar.setClassName("subject-link-button");
        HorizontalLayout layout = new HorizontalLayout();
        layout.setAlignItems(Alignment.CENTER);
        layout.add(new H3(Transl.get("Subjects")));
        layout.add(VaadinIcon.CHEVRON_DOWN.create());

        MenuItem menuItem = menuBar.addItem(layout);
        list.forEach(subjectDto -> {
            MenuItem subMenuItem = menuItem.getSubMenu().addItem(subjectDto.getName());
            subMenuItem.addClickListener(menuItemClickEvent ->
                    UI.getCurrent().navigate(SubjectDetailView.ROUTE.concat("/").concat(subjectDto.getId())));
        });
        header.add(menuBar);
    }

    public void showSubjectLink(SubjectDto subjectDto) {
        if (subjectDto != null) {
            header.add(VaadinComponents.getSubjectLink(subjectDto));
        }
    }

    public CenteredHorizontalLayout getCenteredHorizontalLayout() {
        return centeredHorizontalLayout;
    }
}
