package cz.bbn.cerberus.note.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.EditEvent;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideBarCountUpdateAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.HashSet;
import java.util.Set;


public class NoteComponent extends VerticalLayout {

    private final NoteComponentOperation noteComponentOperation;
    private final Checkbox showArchived;
    private final String id;
    private final AppEnv appEnv;
    private final NoteTypeEnum noteTypeEnum;
    private final Permission viewPermission;
    private final Permission editPermission;
    private final boolean canViewCustomPermission;
    private final boolean canEditCustomPermission;

    private NoteGridComponent noteGridComponent;
    private NoteFilterComponent noteFilterComponent;
    private final CountIntIndicator noteIndicator;

    public NoteComponent(NoteComponentOperation noteComponentOperation,
                         Checkbox showArchived, String id,
                         AppEnv appEnv, NoteTypeEnum noteTypeEnum,
                         Permission viewPermission, Permission editPermission,
                         boolean canViewCustomPermission, boolean canEditCustomPermission,
                         CountIntIndicator noteIndicator) {
        this.noteComponentOperation = noteComponentOperation;
        this.showArchived = showArchived;
        this.id = id;
        this.appEnv = appEnv;
        this.noteTypeEnum = noteTypeEnum;
        this.viewPermission = viewPermission;
        this.editPermission = editPermission;
        this.canViewCustomPermission = canViewCustomPermission;
        this.canEditCustomPermission = canEditCustomPermission;
        this.noteIndicator = noteIndicator;
        init();
    }

    public NoteFilterDto getFilterDto() {
        return noteFilterComponent.getFilter();
    }

    private void init() {
        if ((id != null || noteTypeEnum != null) && (SecurityUtils.hasPermission(viewPermission) || noteTypeEnum.equals(NoteTypeEnum.ANY))
                && canViewCustomPermission) {

            HorizontalLayout horizontalLayout = new HorizontalLayout();

            Button buttonNew = VaadinComponents.getNewButton(Transl.get("New note"));
            if (SecurityUtils.hasPermission(editPermission) && SecurityUtils.hasCustomPermission(
                    editPermission.getObjectName(), id, editPermission.name())) {
                horizontalLayout.add(buttonNew);
            }
            horizontalLayout.setAlignItems(Alignment.CENTER);

            this.add(horizontalLayout);

            Button search = VaadinComponents.getSearchButton();
            Set<String> permissionSet = new HashSet<>();
            permissionSet.add(viewPermission.name());

            if (id != null) {
                noteFilterComponent = new NoteFilterComponent(search,
                        noteComponentOperation.getUserListForObject(id, noteTypeEnum), permissionSet, noteTypeEnum);
                this.add(noteFilterComponent);
                noteFilterComponent.setVisible(false);

                noteGridComponent = new NoteGridComponent(
                        noteComponentOperation.getDeleteAction(noteIndicator, noteTypeEnum, id),
                        appEnv,
                        noteComponentOperation.getItemsAction(id, showArchived, noteTypeEnum, noteFilterComponent),
                        getEditEvent(),
                        noteComponentOperation.getNoteDtoSaveAction(id, noteTypeEnum),
                        noteComponentOperation.getItemAction(), noteComponentOperation.getListService(),
                        noteComponentOperation);
            } else {
                noteFilterComponent = new NoteFilterComponent(
                        search, noteComponentOperation.getListService().getUserDtoList(),
                        Permission.getNoteReadPermissionSet(), noteTypeEnum);
                this.add(noteFilterComponent);
                noteFilterComponent.setVisible(false);

                noteGridComponent = new NoteGridComponent(noteComponentOperation.getDeleteAction(), appEnv,
                        noteComponentOperation.getItemsAction(noteFilterComponent), getEditEvent(),
                        noteComponentOperation.getNoteDtoSaveAction(), noteComponentOperation.getItemAction(),
                        noteComponentOperation.getListService(), noteComponentOperation);
            }
            noteGridComponent.loadData();
            noteGridComponent.setHeight("calc(100% - 2.5em)");

            Button showFilter = VaadinComponents.getShowFilterButton();
            Button hideFilter = VaadinComponents.getHideFilterButton();
            hideFilter.setVisible(false);

            showFilter.addClickListener(e -> {
                noteFilterComponent.setVisible(true);
                noteGridComponent.setHeight("calc(100% - 12em)");
                showFilter.setVisible(false);
                hideFilter.setVisible(true);
            });

            hideFilter.addClickListener(e -> {
                noteFilterComponent.setVisible(false);
                noteGridComponent.setHeight("calc(100% - 2.5em)");
                showFilter.setVisible(true);
                hideFilter.setVisible(false);
            });

            horizontalLayout.add(showFilter, hideFilter, showArchived);

            showArchived.addClickListener(checkboxClickEvent -> noteGridComponent.loadData());

            search.addClickListener(e -> noteGridComponent.loadData());

            buttonNew.addClickListener(buttonClickEvent -> dialogAction(new NoteDto()));
            this.add(noteGridComponent);
            this.setHeightFull();
        }
    }

    private EditEvent getEditEvent() {
        return itemId -> {
            final NoteDto noteDto = noteComponentOperation.getItemAction().getItem(itemId);
            return buttonClickEvent -> dialogAction(noteDto);
        };
    }

    private void dialogAction(NoteDto noteDto) {
        EditNoteDialog editNoteDialog = new EditNoteDialog(noteDto,
                noteComponentOperation.getNoteDtoSaveAction(id, noteTypeEnum), noteGridComponent,
                canEditCustomPermission, getCountUpdateAction());
        editNoteDialog.open();
    }

    private SlideBarCountUpdateAction getCountUpdateAction() {
        return () -> noteIndicator.setCount(noteComponentOperation.getNoteCountByTypeAndObjectId(noteTypeEnum, id));
    }

}
