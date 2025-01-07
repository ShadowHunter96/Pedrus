package cz.bbn.cerberus.note.ui.component;

import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.EditEvent;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.translation.Transl;

public class NoteGridComponent extends AppInfiniteGrid<NoteDto> {

    private final EditEvent editEvent;

    private final SaveAction<NoteDto> noteSaveAction;
    private final GetItemAction<NoteDto> getItemAction;
    private final ListService listService;
    private final NoteComponentOperation componentService;

    public NoteGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<NoteDto> itemsAction,
                             EditEvent editEvent, SaveAction<NoteDto> noteSaveAction,
                             GetItemAction<NoteDto> getItemAction, ListService listService,
                             NoteComponentOperation componentService) {
        super(deleteAction, appEnv, itemsAction);
        this.editEvent = editEvent;
        this.noteSaveAction = noteSaveAction;
        this.getItemAction = getItemAction;
        this.listService = listService;
        this.componentService = componentService;
        initGrid();
    }

    private void initGrid() {
        addColumn(NoteDto::getNote).setHeader(Transl.get("note")).setSortable(true).setKey("note");
        addColumn(noteDto -> AppUtils.formatDateTime(noteDto.getDate(), true))
                .setHeader(Transl.get("Created date")).setSortable(true).setKey("date");
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Created by"))
                .setSortable(true).setKey("userEntity");
        if (listService != null) {
            addColumn(new ComponentRenderer<>(this::getEntityName)).setHeader(Transl.get("Entity"))
                    .setSortable(true).setKey("entityId");
        }
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private Span getUserName(NoteDto dto) {
        if (dto.getUserDto() != null) {
            if (dto.getUserDto().getAcronym() != null && !dto.getUserDto().getAcronym().isEmpty()) {
                return new Span(dto.getUserDto().getAcronym());
            } else {
                return new Span(dto.getUserDto().getName());
            }
        }
        return new Span();
    }

    private HorizontalLayout getGridButtons(NoteDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit note",
                "Are you sure you want to delete note ?", "Delete note");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                clickedItem.getType().getEditPerm(),
                clickedItem.getType().getDeletePerm(), clickedItem.getId().toString(),
                "", "", false);
        return getSimpleGridButtons(dataVariables, stringVariables, this, editEvent);
    }

    private void gridClicked(Long id) {
        NoteDto noteDto = getItemAction.getItem(String.valueOf(id));
        EditNoteDialog editNoteDialog = new EditNoteDialog(noteDto, noteSaveAction, this, getCanEdit(noteDto));
        editNoteDialog.open();
    }

    private Span getEntityName(NoteDto clickedItem) {
        if (clickedItem.getEntityId() != null && clickedItem.getType() != null) {
            return new Span(listService.getNameByIdAndObjectType(clickedItem.getEntityId(),
                    clickedItem.getType().getObjectName()));
        }
        return new Span();
    }

    private boolean getCanEdit(NoteDto noteDto) {
        return SecurityUtils.hasCustomPermission(noteDto.getType().getObjectName(), noteDto.getEntityId(),
                noteDto.getType().getEditPerm().name());
    }
}
