package cz.bbn.cerberus.subject.ui.component.tab;

import com.vaadin.flow.component.html.H4;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.labelsubject.LabelSubjectComponentOperation;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.labelsubject.ui.component.LabelSubjectGridComponent;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;


public class SubjectInfoTab extends TabSimpleComponent {

    private final AppEnv appEnv;
    private final LabelSubjectComponentOperation labelSubjectComponentOperation;
    private final SubjectDto dto;

    private LabelSubjectGridComponent labelSubjectGridComponent;

    public SubjectInfoTab(SubjectDto dto, AppEnv appEnv,
                          LabelSubjectComponentOperation labelSubjectComponentOperation) {
        this.appEnv = appEnv;
        this.labelSubjectComponentOperation = labelSubjectComponentOperation;
        this.dto = dto;
        initTab();
    }

    private void initTab() {
        H4 labelSubject = new H4(Transl.get("Label subject list"));
        this.add(labelSubject);

        labelSubjectGridComponent =
                new LabelSubjectGridComponent(labelSubjectComponentOperation.getDeleteAction(dto.getId()), appEnv,
                        labelSubjectComponentOperation.getItemsAction(dto.getId()));
        this.add(labelSubjectGridComponent);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        labelSubjectGridComponent.loadData();
    }


    public AppInfiniteGrid<LabelSubjectDto> getGrid() {
        return labelSubjectGridComponent;
    }


}
