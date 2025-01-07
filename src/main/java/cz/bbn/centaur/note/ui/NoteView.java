package cz.bbn.cerberus.note.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.EditEvent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.note.ui.component.EditNoteDialog;
import cz.bbn.cerberus.note.ui.component.NoteFilterComponent;
import cz.bbn.cerberus.note.ui.component.NoteGridComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;


@Route(value = NoteView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.NOTE_VIEW)
@Slf4j
public class NoteView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "note-list";

    private final NoteComponentOperation noteComponentOperation;
    private final ListService listService;
    private final UserService userService;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    private NoteGridComponent noteGridComponent;

    public NoteView(NoteComponentOperation noteComponentOperation, ListService listService, UserService userService,
                    AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.noteComponentOperation = noteComponentOperation;
        this.listService = listService;
        this.userService = userService;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }


    private void initView(String params) {
        Button searchButton = VaadinComponents.getSearchButton();

        NoteFilterComponent noteFilterComponent = new NoteFilterComponent(searchButton, userService.findUserList(),
                noteComponentOperation, Permission.getNoteReadPermissionSet(), params, getHistoryBreadcrumbs());

        noteGridComponent = new NoteGridComponent(noteComponentOperation.getDeleteAction(), appEnv,
                noteComponentOperation.getItemsAction(noteFilterComponent), getEditEvent(),
                noteComponentOperation.getNoteDtoSaveAction(), noteComponentOperation.getItemAction(),
                listService, noteComponentOperation);
        noteGridComponent.loadData();

        searchButton.addClickListener(e -> {
            noteGridComponent.loadData();
            noteFilterComponent.fillUrl();
        });

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Note list"), entityNewComponentOperation,
                null, ObjectType.ANY);

        card.addClassName(RobotFrameworkVariables.NOTE_VIEW_CARD_ID.getValue());

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        card.add(noteFilterComponent, noteGridComponent);

        add(card);
    }

    private EditEvent getEditEvent() {
        return itemId -> {
            final NoteDto noteDto = noteComponentOperation.getItemAction().getItem(itemId);
            return buttonClickEvent -> dialogAction(noteDto);
        };
    }

    private void dialogAction(NoteDto noteDto) {
        EditNoteDialog editNoteDialog = new EditNoteDialog(noteDto,
                noteComponentOperation.getNoteDtoSaveAction(noteDto.getEntityId(), noteDto.getType()),
                noteGridComponent, SecurityUtils.hasCustomPermission(
                noteDto.getType().getObjectName(), noteDto.getEntityId(),
                noteDto.getType().getEditPerm().name()));
        editNoteDialog.open();
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}