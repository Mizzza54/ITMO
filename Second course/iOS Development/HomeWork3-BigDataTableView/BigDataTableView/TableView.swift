import UIKit

protocol TableViewDataSource: AnyObject {
    func numberOfRowInTableView(_ tableView: TableView) -> Int
    func tableView(_ tableView: TableView, textForRow row: Int) -> String
}

class TableView: UIScrollView {
    weak var dataSource: TableViewDataSource?
    
    var cellArray: [TableCell] = []
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        commonInit()
    }
    
    required init?(coder: NSCoder) {
        super.init(coder: coder)
        commonInit()
    }
    
    private func commonInit() {
        backgroundColor = .systemGreen
        indicatorStyle = .black
        clipsToBounds = true

        cellArray.append(TableCell())
        insertSubview(cellArray[0], at: 0)
    }
    
    
    
    override func layoutSubviews() {
        super.layoutSubviews()
        contentSize = CGSize(width: Int(bounds.width), height: 40 * (dataSource?.numberOfRowInTableView(self))!)
        
        drawCells()
    }

    func drawCells() -> Void {
        let cellWidth = self.bounds.width
        let cellHeight: Int = 40
        let numberOfCells = Int(bounds.height / CGFloat(cellHeight)) + 2
        
        if (cellArray.count < numberOfCells) {
            for _ in 1...numberOfCells {
                cellArray.append(TableCell())
                insertSubview(cellArray.last!, at: 0)
            }
        }
        
        let offset = cellArray[0].frame == CGRect.zero && Int(safeAreaInsets.top) / cellHeight > 0 ? Int(bounds.origin.y / 40) + 1 : Int(bounds.origin.y / 40)
        let from = 1 + offset
        let to = numberOfCells + offset

        if (from > 0 && to <= (dataSource?.numberOfRowInTableView(self))!) {
            for i in from...to {
                let index = i - 1 - offset
                cellArray[index].frame = CGRect(x: 0, y: cellHeight * (i - 1), width: Int(cellWidth), height: cellHeight)
                cellArray[index].update(text: (dataSource?.tableView(self, textForRow: i))!)
            }
        }
    }
}
