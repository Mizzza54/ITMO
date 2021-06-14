import SwiftUI

class TodoListViewModel: ObservableObject {
    @Published var arrayOfTasks: [String] = []
    @Published var isStrikethrough: [Bool] = []
    @Published var text = ""
    @Published var sizeArray = 0
    
    func addTask(text: String) {
        arrayOfTasks.append(text)
        isStrikethrough.append(false)
        sizeArray+=1
    }
    
    func clickOnTask(index: Int) {
        isStrikethrough[index] = !isStrikethrough[index]
    }
}
