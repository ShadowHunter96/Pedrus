package cz.bbn.cerberus.note.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideBarCountUpdateAction;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;

public class EditNoteDialog extends AppDialog {

    private final NoteDto dto;
    private final NoteDto originalDto;
    private final SaveAction<NoteDto> saveAction;
    private final AppInfiniteGrid<NoteDto> grid;
    private final boolean canEditCustomPermission;

    private SlideBarCountUpdateAction slideBarCountUpdateAction;

    public EditNoteDialog(NoteDto noteDto, SaveAction<NoteDto> saveAction, AppInfiniteGrid<NoteDto> grid,
                          boolean canEditCustomPermission) {
        this.dto = noteDto;
        this.saveAction = saveAction;
        this.grid = grid;
        this.canEditCustomPermission = canEditCustomPermission;
        originalDto = SerializationUtils.clone(dto);
        initComponent();
    }

    public EditNoteDialog(NoteDto noteDto, SaveAction<NoteDto> saveAction, AppInfiniteGrid<NoteDto> grid,
                          boolean canEditCustomPermission, SlideBarCountUpdateAction slideBarCountUpdateAction) {
        this.dto = noteDto;
        this.saveAction = saveAction;
        this.grid = grid;
        this.canEditCustomPermission = canEditCustomPermission;
        originalDto = SerializationUtils.clone(dto);
        this.slideBarCountUpdateAction = slideBarCountUpdateAction;
        initComponent();
    }

    private void initComponent() {

        this.setMinWidth("25em");
        this.setWidth("30%");
        Binder<NoteDto> binder = new Binder<>();
        setTitle(dto.getId() == null ? Transl.get("New note") : Transl.get("Edit note"));
        VerticalLayout verticalLayout = new VerticalLayout();
        if (dto.getId() != null) {
            TextField date = new TextField(Transl.get("Created date"));
            date.setReadOnly(true);
            date.setValue(AppUtils.formatDateTime(dto.getDate(), true));


            TextField createdByUser = new TextField(Transl.get("Created by"));
            if (dto.getUserDto() != null) {
                createdByUser.setValue(StringUtils.trimToEmpty(dto.getUserDto().getName()));
            }
            createdByUser.setReadOnly(true);
            verticalLayout.add(date, createdByUser);
        }

        TextArea note = new TextArea(Transl.get("Note"));
        note.setWidthFull();
        binder.forField(note).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(NoteDto::getNote, NoteDto::setNote);
        verticalLayout.add(note);
        note.setWidthFull();
        note.setHeight("10em");
        note.setMaxHeight("10em");
        note.setMaxLength(255);

        FormLayout formLayout = new FormLayout();
        formLayout.setWidthFull();

        Checkbox priority = new Checkbox(Transl.get("Priority message"));
        binder.forField(priority).bind(NoteDto::getPriority, NoteDto::setPriority);
        formLayout.add(priority);

        Checkbox archived = new Checkbox(Transl.get("Archive"));
        binder.forField(archived).bind(NoteDto::getArchived, NoteDto::setArchived);
        formLayout.add(archived);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        verticalLayout.add(formLayout);

        binder.setBean(dto);

        setContent(verticalLayout);

        if (dto.getId() != null && !SecurityUtils.getCurrentUserId().equals(dto.getUserDto().getId())) {
            note.setReadOnly(true);
        }

        if (!canEditCustomPermission) {
            note.setReadOnly(true);
            priority.setReadOnly(true);
            archived.setReadOnly(true);
        }

        Button save = VaadinComponents.getSubmitButton();
        save.setDisableOnClick(true);
        save.addClickListener(buttonClickEvent -> {
            saveAction.saveItem(dto, originalDto);
            grid.loadData();
            if (slideBarCountUpdateAction != null) {
                slideBarCountUpdateAction.updateCount();
            }
            save.setEnabled(true);
            this.close();
        });

        addCloseButton();
        if (canEditCustomPermission) {
            addButtons(save);
        }
    }
}
